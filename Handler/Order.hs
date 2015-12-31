
module Handler.Order (getOrderR, postOrderR, getOrderCompleteR) where

import Import
import Web.Stripe
import Web.Stripe.Charge
import Web.Stripe.Error

import qualified Network.Mail.Mime as Mail
import Text.Shakespeare.Text (stext)

lookupQuantity :: ProductId -> OrderCookie -> Int
lookupQuantity p (OrderCookie c) = case find ((==) p . fst) c >>= return . snd of
    Just a -> a
    Nothing -> 0

query :: Handler [(ProductId, Text, Money, Int)]
query = do
    d <- getDeployment
    cookie <- lookupCookie "order"
    case parseOrder =<< cookie of
        Just oc@(OrderCookie cookie') -> do
            xs <- runDB $ select $ from $ \(c `InnerJoin` p) -> do
                on (c ^. CategoryId ==. p ^. ProductCategory)
                where_ (p ^. ProductId `in_` valList (fmap fst cookie')
                        &&. val d ==. c ^. CategoryDeployment)
                return
                    ( p ^. ProductId
                    , p ^. ProductName
                    , p ^. ProductPrice
                    )
            let addq (Value a, Value b, Value c) = (a, b, c, lookupQuantity a oc)
            return $ fmap addq xs
        Nothing -> return []

calculateAmount :: [(ProductId, Text, Money, Int)] -> Money
calculateAmount = foldr (\(_, _, p, q) t -> p * (Money q) + t) 0

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

queryStripeConfig :: EntityField Deployment Text -> Handler Text
queryStripeConfig field = do
    did <- getDeployment
    result <- runDB $ select $ from $ \d -> do
        where_ (d ^. DeploymentId ==. val did)
        return (d ^. field)
    return $ case result of
        ((Value key):[]) -> key
        _ -> error "queryStripeConfig"

checkout :: Money -> Widget
checkout (Money amount) = do
    addScriptRemote "https://checkout.stripe.com/checkout.js"
    key <- handlerToWidget $ queryStripeConfig DeploymentStripePublic
    $(widgetFile "checkout")

runStripe :: Int -> Handler (Either StripeError (StripeReturn CreateCharge))
runStripe amount = do
    token <- runInputPost $ TokenId <$> ireq textField "co-token"
    secret <- queryStripeConfig DeploymentStripeSecret
    let config = StripeConfig . StripeKey . encodeUtf8 $ secret
    liftIO $ stripe config $ createCharge (Amount amount) GBP -&- token

handlePayment :: Handler Bool
handlePayment = do
    famount <- runInputPost $ ireq intField "co-amount"
    rows <- query
    let (Money vamount) = calculateAmount rows
    if vamount == famount && vamount >= 20
        then do
            c <- lookupCookie "card"
            if maybe False (== "true") c
                then do
                    sr <- runStripe vamount
                    case sr of
                        Left e -> error $ show e
                        Right _ -> return True
                else return False
        else error "u wot m8" -- TODO: Add "oops prices have changes since you made choice" page

getOrderR :: Handler Html
getOrderR = handleOrder Nothing

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
    let amount@(Money a) = calculateAmount rows
        co = if maybe False (== "true") c
            then checkout amount
            else toWidget [hamlet|
<input type="hidden" name="co-amount" value="#{a}">
|]
    if a >= 20
        then defaultLayout $ do
            setTitle "Order"
            $(widgetFile "order")
        else redirect MenuR

postOrderR :: Handler Html
postOrderR = do
    c <- lookupCookie "deliver"
    if maybe False (== "true") c then postOrderDeliver else postOrderCollect

postOrderCollect :: Handler Html
postOrderCollect = do
    ((fr, fw), _) <- runFormPost $ phoneForm Nothing
    case fr of
        FormSuccess phone -> do
            card <- handlePayment
            saveOrder card False phone Nothing
        _ -> handleOrder $ Just fw

postOrderDeliver :: Handler Html
postOrderDeliver = do
    ((fr, fw), _) <- runFormPost $ bothForm Nothing
    case fr of
        FormSuccess (addr, phone) -> do
            card <- handlePayment
            runDB (insert addr) >>= saveOrder card True phone . Just
        _ -> handleOrder $ Just fw

saveOrder :: Bool -> Bool -> Phone -> Maybe AddressId -> Handler Html
saveOrder card deliver p ma = do
    d <- getDeployment
    u <- maybeAuthId
    o <- runDB $ insert $ Order d u card deliver False ma p
    ps <- query
    _ <- forM ps $ \(pid, _, c, q) ->
        runDB $ insert $ OrderLine o pid c q
    redirect OrderCompleteR

getOrderCompleteR :: Handler Html
getOrderCompleteR = do
    deleteCookie "order" "/"
    deleteCookie "deliver" "/"
    deleteCookie "card" "/"
    master <- getYesod
    d <- getDeployment
    r <- runDB $ select $ from $ \t -> do
        where_ $ t ^. DeploymentId ==. val d
        return $ t ^. DeploymentEmail
    let masterEmail = appEmail $ appSettings master
        email = case r of
            ((Value m):[]) -> m
            _ -> error "getOrderCompleteR"
    liftIO $ Mail.renderSendMail $ Mail.simpleMail'
        (Mail.Address Nothing email)
        (Mail.Address Nothing masterEmail)
        "New Order"
        [stext|
            There is new orderz!!
        |]
    defaultLayout $ setTitle "Thank You" >> toWidget [whamlet|Success|]
