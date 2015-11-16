
module Handler.Product where

import Import hiding (Value, on, (==.))
import Database.Persist.Sql (toSqlKey)
import Database.Esqueleto

form :: OptionList CategoryId -> Form Product
form cats = renderDivs $ Product
    <$> areq (selectField (return cats)) "Category" Nothing
    <*> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq moneyField "Price" Nothing

queryProudctList :: Handler [(Value Text, Value Money, Value Text)]
queryProudctList = do
    d <- getDeployment
    runDB $ select $ from $ \(c `LeftOuterJoin` p) -> do
        on (c ^. CategoryId ==. p ^. ProductCategory)
        where_ (c ^. CategoryDeployment ==. (val d))
        return
            ( p ^. ProductName
            , p ^. ProductPrice
            , c ^. CategoryName
            )

queryCategoryList :: Handler [(Value Text, Value CategoryId)]
queryCategoryList = do
    d <- getDeployment
    runDB $ select $ from $ \c -> do
        where_ (c ^. CategoryDeployment ==. (val d))
        return (c ^. CategoryName, c ^. CategoryId)

getProductR :: Handler Html
getProductR = do
    deployment <- getDeployment
    ps <- queryProudctList
    defaultLayout $(widgetFile "product-list")

getProductNewR :: Handler Html
getProductNewR = do
    cs <- queryCategoryList
    cs' <- optionsPairs $ fmap (\(a, b) -> (unValue a, unValue b)) cs
    ((result, widget), enc) <- runFormPost $ form cs'
    defaultLayout $(widgetFile "product-edit")

postProductNewR :: Handler Html
postProductNewR = getProductNewR

getProductEditR :: Key Product -> Handler Html
getProductEditR p = notFound

postProductEditR :: Key Product -> Handler Html
postProductEditR p = notFound
