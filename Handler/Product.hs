
module Handler.Product where

import Import
import qualified Data.Aeson as Json

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

queryProudctList :: Handler [(Value ProductId, Value Text, Value Money, Value Text, Value Bool, Value (Maybe Text))]
queryProudctList = do
    d <- getDeploymentId
    runDB $ select $ from $ \(p `InnerJoin` c `LeftOuterJoin` pt `LeftOuterJoin` t) -> do
        on $ t ?. TagId ==. pt ?. ProductTagTag
        on $ just( p ^. ProductId) ==. pt ?. ProductTagProduct
        on $ c ^. CategoryId ==. p ^. ProductCategory
        where_ (c ^. CategoryDeployment ==. (val d))
        return $ foldr categorise [] rows
      where
        categorise (Value key, Value product, Value price, Value category, Value available, Value name) (x@(current, sublist):xs) =
            if cat == current
                then (cat, (fromSqlKey key, name, price):sublist):xs
                else (cat, [(fromSqlKey key, name, price)]):x:xs
        categorise (Value key, Value product, Value price, Value category, Value available, Value name) [] =
            [(fromSqlKey key, product, price, category, available, [name])]

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
    <*> pure (maybe False productAvailable p)

cform :: Form Category
cform = renderDivs $ Category
    <$> lift getDeploymentId
    <*> areq textField "Name" Nothing
    <*> areq intField "Order" Nothing

getProductR :: Handler Html
getProductR = do
    ps <- queryProudctList
    defaultLayout $ setTitle "Products" >> $(widgetFile "product-list")

getProductNewR :: Handler Html
getProductNewR = do
    ((result, widget), enc) <- runFormPost $ form Nothing
    case result of
        FormSuccess p -> do
            _ <- runDB $ insert p
            addMessage "success" "Created new product"
            redirect ProductR
        _ -> defaultLayout $ setTitle "New Product" >> $(widgetFile "product-new")

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
                    addMessage "success" "Product Updated"
                    redirect ProductR
                _ -> defaultLayout $ setTitle "Edit Product" >> $(widgetFile "product-edit")

postProductEditR :: Key Product -> Handler Html
postProductEditR key = getProductEditR key

postAjaxProductAvailableR :: ProductId -> Handler Json.Value
postAjaxProductAvailableR pid = do
    b <- getJson return
    d <- getDeploymentId
    r <- runDB $ select $ from $ \(p `InnerJoin` c) -> do
        on $ c ^. CategoryId ==. p ^. ProductCategory
        where_ $ p ^. ProductId ==. val pid
        return $ c ^. CategoryDeployment

    case r of
        ((Value d'):[]) -> if d == d' then return () else notFound
        _ -> notFound

    runDB $ update $ \p -> do
        set p [ ProductAvailable =. val b ]
        where_ $ p ^. ProductId ==. val pid

    return Json.Null
