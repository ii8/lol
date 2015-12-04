
module Handler.Order where

import Import
import Web.Stripe
import Web.Stripe.Charge
import Web.Stripe.Error

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

addressForm :: Maybe Address -> Form Address
addressForm a = renderDivs $ Address
    <$> aopt textField "Name" (addressName <$> a)
    <*> areq textField "Address Line 1" (addressLineone <$> a)
    <*> aopt textField "Address Line 2" (addressLinetwo <$> a)
    <*> areq textField "Town" (addressTown <$> a)
    <*> areq textField "County" (addressCounty <$> a)
    <*> areq textField "Postcode" (addressPostcode <$> a)

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

handleOrder :: Maybe Widget -> Handler Html
handleOrder maddr = do
    rows <- query
    addr <- case maddr of
        Just a -> return a
        Nothing -> return . fst =<< generateFormPost (addressForm Nothing)
    let co = checkout (Money 232)
    defaultLayout $ do
        setTitle "Order"
        $(widgetFile "order")

runStripe :: Handler (Either StripeError (StripeReturn CreateCharge))
runStripe = do
    token <- runInputPost $ TokenId <$> ireq textField "co-token"
    amount <- runInputPost $ ireq intField "co-amount"
    secret <- queryStripeConfig DeploymentStripeSecret
    let config = StripeConfig . StripeKey . encodeUtf8 $ secret
    liftIO $ stripe config $ createCharge (Amount amount) GBP -&- token

stripeError :: StripeError -> Handler Html
stripeError err = defaultLayout $ toWidget [whamlet|Error: #{show err}|]

getOrderR :: Handler Html
getOrderR = handleOrder Nothing

postOrderR :: Handler Html
postOrderR = do
    ((fr, aw), _) <- runFormPost $ addressForm Nothing
    case fr of
        FormSuccess addr -> do
            sr <- runStripe
            case sr of
                Left err -> stripeError err
                Right _ -> runDB (insert addr) >>= saveOrder . Just
        FormMissing -> do
            sr <- runStripe
            case sr of
                Left err -> stripeError err
                Right _ -> saveOrder Nothing
        FormFailure _ -> handleOrder (Just aw)

saveOrder :: Maybe AddressId -> Handler Html
saveOrder ma = do
    d <- getDeployment
    u <- maybeAuthId
    o <- runDB $ insert $ Order d u ma
    ps <- query
    _ <- forM ps $ \(pid, _, p, q) ->
        runDB $ insert $ OrderLine o pid p q
    redirect OrderCompleteR

getOrderCompleteR :: Handler Html
getOrderCompleteR = do
    deleteCookie "order" "/"
    defaultLayout $ setTitle "Thank You" >> toWidget [whamlet|Success|]
