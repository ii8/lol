
module Handler.Order
    ( getOrderR
    , postOrderR
    , postAjaxOrderCompleteR
    , postAjaxOrderCancelR
    , postAjaxOrderRefundR
    ) where

import Import
import Text.Hamlet (hamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Database.Esqueleto.Internal.Language (Update)
import qualified Data.Aeson as Json
import Web.Stripe
import Web.Stripe.Refund
import Network.HTTP.Types.Status (badRequest400)

orderList :: Handler [
    ( Value OrderId
    , Value Phone
    , Value Bool
    , Value Bool
    , Value OrderStatus
    , Value PaymentStatus
    , Maybe (Entity Address)
    )]
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

addressList :: Handler (OptionList AddressId)
addressList = do
    d <- getDeploymentId
    as <- runDB $ select $ from $ \(da `InnerJoin` a) -> do
        on $ da ^. DeploymentAddressAddress ==. a ^. AddressId
        where_ (da ^. DeploymentAddressDeployment ==. (val d))
        return (a ^. AddressName, a ^. AddressPostcode, a ^. AddressId)
    optionsPairs $ fmap (\(a, b, c) ->
        (maybe "" (flip mappend ", ") (unValue a) <> unValue b
        , unValue c)) as

orderTable :: Handler Html
orderTable = do
    rows <- orderList
    withUrlRenderer $(hamletFile "templates/order-table.hamlet")

formOrder :: Maybe Order -> Form Order
formOrder o = renderDivs $ Order
    <$> lift getDeploymentId
    <*> pure Nothing
    <*> areq (radioField cardList) "Payment Method" (orderCard <$> o)
    <*> areq (radioField deliverList) "Collect or deliver" (orderDeliver <$> o)
    <*> areq (radioField optionsEnum) "Order Status" (Just $ maybe New orderStatus o)
    <*> areq (radioField optionsEnum) "Payment Status" (Just $ maybe Payable orderPayment o)
    <*> aopt (selectField addressList) "Address" (orderAddress <$> o)
    <*> areq phoneField "Phone number" (orderPhone <$> o)
    <*> pure Nothing
  where
    cardList = optionsPairs [("Cash" :: Text, False), ("Card", True)]
    deliverList = optionsPairs [("Collect" :: Text, False), ("Deliver", True)]

formAddr :: Maybe Address -> Form Address
formAddr a = renderDivs $ Address
    <$> aopt textField "Name" (addressName <$> a)
    <*> areq textField "Address Line 1" (addressLineone <$> a)
    <*> aopt textField "Address Line 2" (addressLinetwo <$> a)
    <*> areq textField "Town" (addressTown <$> a)
    <*> areq textField "County" (addressCounty <$> a)
    <*> areq textField "Postcode" (addressPostcode <$> a)

getOrderR :: Handler Html
getOrderR = do
    ((ofr, ofw), oenc) <- runFormPost $ identifyForm "order" $ formOrder Nothing
    ((afr, afw), aenc) <- runFormPost $ identifyForm "addr" $ formAddr Nothing
    case afr of
        FormSuccess a -> do
            d <- getDeploymentId
            aid <- runDB $ insert a
            void $ runDB $ insert $ DeploymentAddress d aid
        _ -> return ()
    case ofr of
        FormSuccess o -> do
            aid <- maybe
                (return Nothing)
                (\aid -> return . Just =<< duplicate aid)
                (orderAddress o)
            let o' = o { orderAddress = aid }
            void $ runDB $ insert o'
        _ -> return ()
    table <- orderTable
    defaultLayout $(widgetFile "order")
  where
    duplicate aid = do
        a <- runDB $ get404 aid
        runDB $ insert a

postOrderR :: Handler Html
postOrderR = getOrderR

updateOrder :: [SqlExpr (Update Order)] -> OrderId -> Handler Json.Value
updateOrder updates key = do
    d <- getDeploymentId
    runDB $ update $ \o -> do
        set o updates
        where_ ( o ^. OrderId ==. val key &&. o ^. OrderDeployment ==. val d )
    returnJson . renderHtml =<< orderTable

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
        return ( o ^. OrderPayment, o ^. OrderCharge )
    case r of
        ((Value Paid, Value (Just c)):[]) -> do
            secret <- deploymentStripeSecret <$> getDeployment
            let config = StripeConfig . StripeKey . encodeUtf8 $ secret
            refund <- liftIO $ stripe config $ createRefund c
            when (isLeft refund) (sendResponseStatus badRequest400 ())
            updateOrder
                [ OrderStatus =. val Cancelled
                , OrderPayment =. val Refunded ]
                key
        _ -> (sendResponseStatus badRequest400 ())
