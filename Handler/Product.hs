
module Handler.Product where

import Import

queryProduct :: ProductId -> Handler (Maybe Product)
queryProduct key = do
    d <- getDeploymentId
    ps <- runDB $ select $ from $ \(c `InnerJoin` p) -> do
        on (c ^. CategoryId ==. p ^. ProductCategory)
        where_ (c ^. CategoryDeployment ==. (val d) &&. p ^. ProductId ==. (val key))
        return p
    return $ case ps of
        ((Entity _ p):_) -> Just p
        [] -> Nothing

queryProudctList :: Handler [(Value ProductId, Value Text, Value Money, Value Text)]
queryProudctList = do
    d <- getDeploymentId
    runDB $ select $ from $ \(c `InnerJoin` p) -> do
        on (c ^. CategoryId ==. p ^. ProductCategory)
        where_ (c ^. CategoryDeployment ==. (val d))
        return
            ( p ^. ProductId
            , p ^. ProductName
            , p ^. ProductPrice
            , c ^. CategoryName
            )

queryCategoryList :: Handler (OptionList CategoryId)
queryCategoryList = do
    d <- getDeploymentId
    cs <- runDB $ select $ from $ \c -> do
        where_ (c ^. CategoryDeployment ==. (val d))
        return (c ^. CategoryName, c ^. CategoryId)
    optionsPairs $ fmap (\(a, b) -> (unValue a, unValue b)) cs

form :: Maybe Product -> Form Product
form p = renderDivs $ Product
    <$> areq (selectField queryCategoryList) "Category" (productCategory <$> p)
    <*> areq textField "Name" (productName <$> p)
    <*> areq textField "Description" (productDescription <$> p)
    <*> areq moneyField "Price" (productPrice <$> p)

cform :: Form Category
cform = renderDivs $ Category
    <$> lift getDeploymentId
    <*> areq textField "Name" Nothing
    <*> areq intField "Order" Nothing

getProductR :: Handler Html
getProductR = do
    ps <- queryProudctList
    defaultLayout $(widgetFile "product-list")

getProductNewR :: Handler Html
getProductNewR = do
    ((result, widget), enc) <- runFormPost $ form Nothing
    case result of
        FormSuccess p -> do
            _ <- runDB $ insert p
            setMessage "Created new product"
            redirect ProductR
        _ -> defaultLayout $(widgetFile "product-new")

postProductNewR :: Handler Html
postProductNewR = getProductNewR

getProductEditR :: Key Product -> Handler Html
getProductEditR key = do
    mp <- queryProduct key
    case mp of
        Nothing -> notFound
        old -> do
            ((result, widget), enc) <- runFormPost $ form old
            case result of
                FormSuccess new -> do
                    runDB $ replace key new
                    setMessage "Product Updated"
                    redirect ProductR
                _ -> defaultLayout $(widgetFile "product-edit")

postProductEditR :: Key Product -> Handler Html
postProductEditR key = getProductEditR key
