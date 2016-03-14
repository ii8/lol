
module Handler.OrderNew (getOrderNewR, postOrderNewR, getOrderCompleteR) where

import Import
import Web.Stripe
import Web.Stripe.Charge
import Web.Stripe.Error

import qualified Network.Mail.Mime as Mail
import Text.Shakespeare.Text (stext)

minAmount :: Int
minAmount = 20

lookupQuantity :: ProductId -> OrderCookie -> Int
lookupQuantity p (OrderCookie c) = case find ((==) p . fst) c >>= return . snd of
    Just a -> a
    Nothing -> 0

query :: Handler [(ProductId, Text, Money, Int)]
query = do
    d <- getDeploymentId
    cookie <- lookupCookie "order"
    case parseOrder =<< cookie of
        Just oc@(OrderCookie cookie') -> do
            xs <- runDB $ select $ from $ \(c `InnerJoin` p) -> do
                on (c ^. CategoryId ==. p ^. ProductCategory)
                where_ (p ^. ProductId `in_` valList (fmap fst cookie')
                        &&. val d ==. c ^. CategoryDeployment
                        &&. p ^. ProductAvailable)
                return
                    ( p ^. ProductId
                    , p ^. ProductName
                    , p ^. ProductPrice
                    )
            let addq (Value a, Value b, Value c) = (a, b, c, lookupQuantity a oc)
            return $ fmap addq xs
        Nothing -> return []

calculateAmount :: [(ProductId, Text, Money, Int)] -> (Int, Money)
calculateAmount = foldr (\(_, _, p, q) t -> (fst t + 1, p * (Money q) + snd t)) (0, 0)

address :: Maybe Address -> AForm Handler Address
address a = Address
    <$> aopt textField "Name" (addressName <$> a)
    <*> areq textField "Address Line 1" (addressLineone <$> a)
    <*> aopt textField "Address Line 2" (addressLinetwo <$> a)
    <*> areq textField "Town" (addressTown <$> a)
    <*> areq textField "County" (addressCounty <$> a)
    <*> areq textField "Postcode" (addressPostcode <$> a)

bothForm :: Maybe (Address, Phone) -> Form (Address, Phone)
bothForm m = renderDivs $ (,)
    <$> address (fst <$> m)
    <*> areq phoneField "Phone Number" (snd <$> m)

phoneForm :: Maybe Phone -> Form Phone
phoneForm p = renderDivs $ areq phoneField "Phone Number" p

checkout :: Int -> Money -> Widget
checkout num (Money amount) = do
    addScriptRemote "https://checkout.stripe.com/checkout.js"
    key <- handlerToWidget $ deploymentStripePublic <$> getDeployment
    d <- handlerToWidget $ getDomain
    $(widgetFile "checkout")

runStripe :: Int -> Handler (Either StripeError (StripeReturn CreateCharge))
runStripe amount = do
    token <- runInputPost $ TokenId <$> ireq textField "co-token"
    secret <- deploymentStripeSecret <$> getDeployment
    let config = StripeConfig . StripeKey . encodeUtf8 $ secret
    liftIO $ stripe config $ createCharge (Amount amount) GBP -&- token

handlePayment :: Handler (Maybe ChargeId)
handlePayment = do
    famount <- runInputPost $ ireq intField "co-amount"
    fnum <- runInputPost $ ireq intField "co-count"
    rows <- query
    let (num, Money vamount) = calculateAmount rows
    -- Note that these products will remain in the cookie, they will just be ignored.
    when (num /= fnum) (setMessage msgAvailable >> redirect MenuR)
    when (vamount /= famount) (setMessage msgPrice >> redirect OrderNewR)
    when (vamount < minAmount) (setMessage msgCheap >> redirect MenuR)
    c <- lookupCookie "card"
    if maybe False (== "true") c
        then do
            sr <- runStripe vamount
            case sr of
                Left e -> error . unpack $ show e
                Right charge -> return . Just $ chargeId charge
        else return Nothing
  where
    msgAvailable = "Sorry, some products you selected are no longer available. Please try again."
    msgPrice = "Sorry, prices have changed since you made your choice. Please review your selection"
    msgCheap = "Your order must be at least 20p"

getOrderNewR :: Handler Html
getOrderNewR = handleOrder Nothing

handleOrder :: Maybe Widget -> Handler Html
handleOrder m = do
    rows <- query
    fw <- case m of
        Just a -> return a
        Nothing -> do
            c <- lookupCookie "deliver"
            case maybe False (== "true") c of
                True -> return . fst =<< generateFormPost (bothForm Nothing)
                False -> return . fst =<< generateFormPost (phoneForm Nothing)
    c <- lookupCookie "card"
    let (num, amount@(Money a)) = calculateAmount rows
        co = if maybe False (== "true") c
            then checkout num amount
            else toWidget [hamlet|
<input type="hidden" name="co-amount" value="#{a}">
<input type="hidden" name="co-count" value="#{num}">
|]
    if a >= minAmount
        then defaultLayout $ do
            setTitle "Order"
            $(widgetFile "order-new")
        else redirect MenuR

postOrderNewR :: Handler Html
postOrderNewR = do
    c <- lookupCookie "deliver"
    if maybe False (== "true") c then postOrderDeliver else postOrderCollect

postOrderCollect :: Handler Html
postOrderCollect = do
    ((fr, fw), _) <- runFormPost $ phoneForm Nothing
    case fr of
        FormSuccess phone -> do
            mcharge <- handlePayment
            saveOrder (isJust mcharge) False phone Nothing mcharge
        _ -> handleOrder $ Just fw

postOrderDeliver :: Handler Html
postOrderDeliver = do
    ((fr, fw), _) <- runFormPost $ bothForm Nothing
    case fr of
        FormSuccess (addr, phone) -> do
            mcharge <- handlePayment
            aid <- runDB $ insert addr
            saveOrder (isJust mcharge) True phone (Just aid) mcharge
        _ -> handleOrder $ Just fw

saveOrder :: Bool -> Bool -> Phone -> Maybe AddressId -> Maybe ChargeId -> Handler Html
saveOrder card deliver p ma charge = do
    d <- getDeploymentId
    u <- maybeAuthId
    let payment = if card then Paid else Payable
    o <- runDB $ insert $ Order d u card deliver New payment ma p charge
    ps <- query
    _ <- forM ps $ \(pid, _, c, q) ->
        runDB $ insert $ OrderLine o pid c q
    completeOrder
    redirect OrderCompleteR

completeOrder :: Handler ()
completeOrder = do
    deleteCookie "order" "/"
    deleteCookie "deliver" "/"
    deleteCookie "card" "/"
    email <- deploymentEmail <$> getDeployment
    masterEmail <- appEmail . appSettings <$> getYesod
    liftIO $ Mail.renderSendMail $ Mail.simpleMail'
        (Mail.Address Nothing email)
        (Mail.Address Nothing masterEmail)
        "New Order"
        [stext|
            There is new orderz!!
        |]

getOrderCompleteR :: Handler Html
getOrderCompleteR = defaultLayout $ do
    setTitle "Thank You"
    toWidget [whamlet|Success|]
