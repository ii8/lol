
module Handler.Order
    ( getOrderR
    , postAjaxOrderCompleteR
    , postAjaxOrderCancelR
    , postAjaxOrderRefundR
    , getAjaxOrderLinesR
    , getAjaxOrderNewR
    ) where

import Import
import Text.Hamlet (hamletFile)
import Database.Esqueleto.Internal.Language (Update)
import qualified Data.Aeson as Json
import Web.Stripe
import Web.Stripe.Refund
import Network.HTTP.Types.Status (badRequest400)
import Text.Blaze.Html.Renderer.Text (renderHtml)

rowsPerPage :: Int64
rowsPerPage = 20

orderList :: Maybe Int -> DeploymentId -> SqlPersistT Handler [(Entity Order, Maybe (Entity Address))]
orderList mp d = do
    case mp of
        Just page -> select $ from $ \(o `LeftOuterJoin` a) -> do
            on $ o ^. OrderAddress ==. a ?. AddressId
            where_ $ o ^. OrderDeployment ==. val d
            orderBy [desc (o ^. OrderOrderDate)]
            limit rowsPerPage
            offset $ (fromIntegral page - 1) * rowsPerPage
            return (o, a)
        _ -> select $ from $ \(o `LeftOuterJoin` a) -> do
            on $ o ^. OrderAddress ==. a ?. AddressId
            where_ $ o ^. OrderDeployment ==. val d
            orderBy [desc (o ^. OrderStatus ==. val New), desc (o ^. OrderOrderDate)]
            limit rowsPerPage
            return (o, a)

latestOrder :: DeploymentId -> SqlPersistT Handler OrderId
latestOrder d = do
    r <- select $ from $ \o -> do
        where_ $ o ^. OrderDeployment ==. val d
        return $ max_ (o ^. OrderId)
    return $ case r of
        (Value oid:[]) -> maybe (toSqlKey 0) id oid
        _ -> toSqlKey 0

orderRow :: Entity Order -> Maybe (Entity Address) -> Template
orderRow entity address = do
    let oid = fromSqlKey (entityKey entity)
        row = entityVal entity
    $(hamletFile "templates/order-row.hamlet")

getOrderR :: Handler Html
getOrderR = do
    d <- getDeploymentId
    Value n <- dbReq $ select $ from $ \o -> do
        where_ $ o ^. OrderDeployment ==. val d
        return countRows
    hmp <- lookupGetParam "page"

    let (n', r) = n `quotRem` rowsPerPage
        num = fromIntegral $ if r > 0 then n' + 1 else n'
        valid x
            | x == 0 || x > num = Nothing
            | otherwise = Just (fromIntegral x)
        mp = valid =<< parseUnsigned =<< hmp
    (latestId, rows) <- runDB $ do
        r' <- orderList mp d
        l <- latestOrder d
        return (l, r')
    defaultLayout $(widgetFile "order")
  where
    list page num
        | num < 14 = [1 .. num]
        | page <= 5 = [1 .. 9] ++ [0, num]
        | page >= num - 4 = 1 : 0 : [num - 8 .. num]
        | otherwise = 1 : 0 : [page - 3 .. page + 3] ++ [0, num]

updateOrder :: [SqlExpr (Update Order)] -> OrderId -> Handler Json.Value
updateOrder updates oid = do
    d <- getDeploymentId
    runDB $ update $ \o -> do
        set o updates
        where_ ( o ^. OrderId ==. val oid &&. o ^. OrderDeployment ==. val d )

    (row, address) <- dbReq $ select $ from $ \(o `LeftOuterJoin` a) -> do
        on $ o ^. OrderAddress ==. a ?. AddressId
        where_ $ o ^. OrderDeployment ==. val d &&. o ^. OrderId ==. val oid
        return (o, a)
    jsonLayout $ orderRow row address

postAjaxOrderCompleteR :: OrderId -> Handler Json.Value
postAjaxOrderCompleteR = updateOrder
    [ OrderStatus =. val Complete
    , OrderPayment =. val Paid ]

postAjaxOrderCancelR :: OrderId -> Handler Json.Value
postAjaxOrderCancelR = updateOrder
    [ OrderStatus =. val Cancelled
    , OrderPayment =. val Unpaid ]

postAjaxOrderRefundR :: OrderId -> Handler Json.Value
postAjaxOrderRefundR key = do
    d <- getDeploymentId
    r <- runDB $ select $ from $ \o -> do
        where_ $ o ^. OrderId ==. val key &&. o ^. OrderDeployment ==. val d
        return ( o ^. OrderPayment, o ^. OrderCard, o ^. OrderCharge )
    case fmap unValue3 r of
        ((Paid, True, (Just c)):[]) -> do
            secret <- deploymentStripeSecret <$> getDeployment
            let config = StripeConfig . StripeKey . encodeUtf8 $ secret
            refund <- liftIO $ stripe config $ createRefund c
            when (isLeft refund) (sendResponseStatus badRequest400 ())
            doUpdate
        ((Paid, False, Nothing):[]) -> doUpdate
        _ -> (sendResponseStatus badRequest400 ())
  where
    doUpdate = updateOrder
        [ OrderStatus =. val Cancelled
        , OrderPayment =. val Refunded ] key

getAjaxOrderLinesR :: OrderId -> Handler Json.Value
getAjaxOrderLinesR order = do
    d <- getDeploymentId
    r <- runDB $ select $ from $ \(o `InnerJoin` l `InnerJoin` p) -> do
        on $ p ^. ProductId ==. l ^. OrderLineProduct
        on $ l ^. OrderLineOrder ==. o ^. OrderId
        where_ $ o ^. OrderId ==. val order &&. o ^. OrderDeployment ==. val d
        return ( p ^. ProductName, l ^. OrderLinePrice, l ^. OrderLineQuantity )
    case fmap unValue3 r of
        [] -> returnJson ("No products in this order" :: Text)
        olines -> do
            let total = foldr (\(_, p, q) t -> p * (Money q) + t) 0 olines
            jsonLayout $(hamletFile "templates/order-lines.hamlet")

getAjaxOrderNewR :: OrderId -> Handler Json.Value
getAjaxOrderNewR oid = do
    d <- getDeploymentId
    (latest, rows) <- runDB $ do
        r <- select $ from $ \(o `LeftOuterJoin` a) -> do
            on $ o ^. OrderAddress ==. a ?. AddressId
            where_ $ o ^. OrderDeployment ==. val d &&. o ^. OrderId >. val oid
            orderBy [desc (o ^. OrderOrderDate)]
            return (o, a)
        l <- latestOrder d
        return (l, r)
    pairs <- jsonLayout $(hamletFile "templates/order-rowpair.hamlet")
    return $ Json.object ["latest" .= latest, "html" .= pairs]
