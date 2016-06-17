
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

{-}
queryProudctList :: Handler [(Text, [(ProductId, Text, Money, Bool, [Maybe Text])])]
queryProudctList = do
    d <- getDeploymentId
    derp <- runDB $ select $ from $ \(p `InnerJoin` c `LeftOuterJoin` pt `LeftOuterJoin` t) -> do
        on $ t ?. TagId ==. pt ?. ProductTagTag
        on $ just( p ^. ProductId) ==. pt ?. ProductTagProduct
        on $ c ^. CategoryId ==. p ^. ProductCategory
        where_ (c ^. CategoryDeployment ==. (val d))
        orderBy [asc (p ^. ProductId)]
        return ( p ^. ProductId
            , t ?. TagName
            , p ^. ProductName
            , p ^. ProductPrice
            , c ^. CategoryName
            , p ^. ProductAvailable
            )
    return $ foldr (groupProducts . groupTags) [] derp -- now implement both
  where

    groupTags (Value pid, Value tag, Value name, Value price, Value category, Value available) [] = [(pid, name, price, category, available, [tag])]
    groupTags (Value pid, Value tag, Value n, Value p, Value c, Value a) allProduct@((lastProduct, _, _, _, _, tags):r) =
        if pid == lastProduct
            then (pid, n, p, c, a, tag:tags):r
            else (pid, n, p, c, a, [tag]):allProduct

    groupProducts (pid, name, price, category, available, [tag]) [] = [(category, [(pid, name, price, available, [tag])])]
    groupProducts (pid, n, p, c, a, [tag]) allCategory@((lastCategory, lastProduct):r) =
        if c == lastCategory
            then (c, (pid, n, p, a, tag):lastProduct):r
            else (c, [(pid, n, p, a, [tag])]):allCategory
            -}
queryProudctList :: Handler [(ProductId, Text, Money, Text, Bool, [Maybe Text])]
queryProudctList = do
    d <- getDeploymentId
    derp <- runDB $ select $ from $ \(p `InnerJoin` c `LeftOuterJoin` pt `LeftOuterJoin` t) -> do -- line 21
        on $ t ?. TagId ==. pt ?. ProductTagTag
        on $ just( p ^. ProductId) ==. pt ?. ProductTagProduct
        on $ c ^. CategoryId ==. p ^. ProductCategory
        where_ (c ^. CategoryDeployment ==. (val d))
        orderBy [asc (p ^. ProductId)]
        return ( p ^. ProductId
            , t ?. TagName
            , p ^. ProductName
            , p ^. ProductPrice
            , c ^. CategoryName
            , p ^. ProductAvailable
            )
    return $ foldr groupProduct [] derp
  where
    groupProduct (Value pid, Value tag, Value name, Value price, Value category, Value available) [] = [(pid, name, price, category, available, [tag])]
    groupProduct (Value pid, Value tag, Value n, Value p, Value c, Value a) allProduct@((lastProduct, _, _, _, _, tags):r) =
        if pid == lastProduct
            then (pid, n, p, c, a, tag:tags):r
            else (pid, n, p, c, a, [tag]):allProduct

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
