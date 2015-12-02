
module Handler.Order where

import Import
import Web.Stripe
import Web.Stripe.Charge

query :: Handler [(Value ProductId, Value Text, Value Money)]
query = do
    d <- getDeployment
    cookie <- lookupCookie "order"
    case parseOrder =<< cookie of
        Just (OrderCookie cookie') -> runDB $ select $ from $ \(c `InnerJoin` p) -> do
            on (c ^. CategoryId ==. p ^. ProductCategory)
            where_ (p ^. ProductId `in_` valList (fmap fst cookie') &&. val d ==. c ^. CategoryDeployment)
            return
                ( p ^. ProductId
                , p ^. ProductName
                , p ^. ProductPrice
                )
        Nothing -> return []

--oform :: Form Order
--oform = renderDivs $ Order
--    <$> lift UserId
--    <*> areq boolField "Deliver" Nothing

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

getOrderR :: Handler Html
getOrderR = do
    rows <- query
    let box = checkout (Money 232)
    defaultLayout $ do
        setTitle "Order"
        $(widgetFile "order")

postOrderR :: Handler Html
postOrderR = do
    token <- runInputPost $ TokenId <$> ireq textField "co-token"
    amount <- runInputPost $ ireq intField "co-amount"
    secret <- queryStripeConfig DeploymentStripeSecret
    let config = StripeConfig . StripeKey . encodeUtf8 $ secret
    result <- liftIO $ stripe config $ createCharge (Amount amount) GBP -&- token
    case result of
        Left err -> defaultLayout $ toWidget [whamlet|Error: #{show err}|]
        Right _ -> deleteCookie "order" "/" >> redirect OrderCompleteR

getOrderCompleteR :: Handler Html
getOrderCompleteR = defaultLayout $ setTitle "Thank You" >> toWidget [whamlet|Success|]
