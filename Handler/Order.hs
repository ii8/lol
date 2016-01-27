
module Handler.Order (getOrderR) where

import Import

orderList :: Handler [(Value Phone, Value Bool, Value Bool, Value OrderStatus, Value PaymentStatus, Maybe (Entity Address))]
orderList = do
    d <- getDeploymentId
    runDB $ select $ from $ \(o `LeftOuterJoin` a) -> do
        on $ o ^. OrderAddress ==. a ?. AddressId
        where_ $ o ^. OrderDeployment ==. val d
        return
            ( o ^. OrderPhone
            , o ^. OrderCard
            , o ^. OrderDeliver
            , o ^. OrderStatus
            , o ^. OrderPayment
            , a)

getOrderR :: Handler Html
getOrderR = orderList >>= \rows -> defaultLayout $(widgetFile "order")
