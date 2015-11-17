
module Handler.Product where

import Import hiding (Value, on, (==.))
import Database.Esqueleto

form :: Form Product
form = renderDivs $ Product
    <$> areq (selectField queryCategoryList) "Category" Nothing
    <*> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq moneyField "Price" Nothing

cform :: Form Category
cform = renderDivs $ Category
    <$> lift getDeployment
    <*> areq textField "Name" Nothing
    <*> areq intField "Order" Nothing

queryProudctList :: Handler [(Value Text, Value Money, Value Text)]
queryProudctList = do
    d <- getDeployment
    runDB $ select $ from $ \(c `InnerJoin` p) -> do
        on (c ^. CategoryId ==. p ^. ProductCategory)
        where_ (c ^. CategoryDeployment ==. (val d))
        return
            ( p ^. ProductName
            , p ^. ProductPrice
            , c ^. CategoryName
            )

queryCategoryList :: Handler (OptionList CategoryId)
queryCategoryList = do
    d <- getDeployment
    cs <- runDB $ select $ from $ \c -> do
        where_ (c ^. CategoryDeployment ==. (val d))
        return (c ^. CategoryName, c ^. CategoryId)
    optionsPairs $ fmap (\(a, b) -> (unValue a, unValue b)) cs

getProductR :: Handler Html
getProductR = do
    ps <- queryProudctList
    defaultLayout $(widgetFile "product-list")

getProductNewR :: Handler Html
getProductNewR = do
    ((result, widget), enc) <- runFormPost $ form
    case result of
        FormSuccess p -> addProduct p >> redirect ProductR
        _ -> defaultLayout $(widgetFile "product-edit")
  where
    addProduct p = do
        _ <- runDB $ insert p
        setMessage "Created new product"

postProductNewR :: Handler Html
postProductNewR = getProductNewR

getProductEditR :: Key Product -> Handler Html
getProductEditR p = notFound

postProductEditR :: Key Product -> Handler Html
postProductEditR p = notFound
