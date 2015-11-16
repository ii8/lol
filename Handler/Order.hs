
module Handler.Order where

import Import hiding (Value, (==.), on)
import Database.Esqueleto

query :: Handler [(Value ProductId, Value Text, Value Money)]
query = do
    d <- getDeployment
    cookie <- lookupCookie "order"
    case parseOrder =<< cookie of
        Just (OrderCookie cookie') -> runDB $ select $ from $ \(c `InnerJoin` p) -> do
            on (c ^. CategoryId ==. p ^. ProductCategory)
            where_ ((p ^. ProductId) `in_` (valList $ fmap fst cookie') &&. (val d) ==. c ^. CategoryDeployment)
            return
                ( p ^. ProductId
                , p ^. ProductName
                , p ^. ProductPrice
                )
        Nothing -> return []

getOrderR :: Handler Html
getOrderR = do
    rows <- query
    defaultLayout $ do
        setTitle "Order"
        $(widgetFile "order")

postOrderR :: Handler Html
postOrderR = notFound
