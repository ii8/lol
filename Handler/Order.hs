
module Handler.Order (getOrderR, postAjaxOrderCompleteR) where

import Import
import Text.Hamlet (hamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Aeson as Json

orderList :: Handler [(Value OrderId,
                       Value Phone,
                       Value Bool,
                       Value Bool,
                       Value OrderStatus,
                       Value PaymentStatus,
                       Maybe (Entity Address))]
orderList = do
    d <- getDeploymentId
    runDB $ select $ from $ \(o `LeftOuterJoin` a) -> do
        on $ o ^. OrderAddress ==. a ?. AddressId
        where_ $ o ^. OrderDeployment ==. val d
        return
            ( o ^. OrderId
            , o ^. OrderPhone
            , o ^. OrderCard
            , o ^. OrderDeliver
            , o ^. OrderStatus
            , o ^. OrderPayment
            , a
            )

orderTable :: Handler Html
orderTable = do
    rows <- orderList
    withUrlRenderer $(hamletFile "templates/order-table.hamlet")

getOrderR :: Handler Html
getOrderR = do
    table <- orderTable
    defaultLayout $(widgetFile "order")

postAjaxOrderCompleteR :: OrderId -> Handler Json.Value
postAjaxOrderCompleteR key = do
    d <- getDeploymentId
    runDB $ update $ \o -> do
        set o [ OrderStatus =. val Complete, OrderPayment =. val Paid ]
        where_ ( o ^. OrderId ==. val key &&. o ^. OrderDeployment ==. val d )
    returnJson . renderHtml =<< orderTable
