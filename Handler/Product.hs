
module Handler.Product where

import Import
import Database.Persist.Sql (toSqlKey)
import Database.Esqueleto as DB

form :: OptionList CategoryId -> Form Product
form cats = renderDivs $ Product
    <$> areq (selectField (return cats)) "Category" Nothing
    <*> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq moneyField "Price" Nothing

queryProudctList :: Handler [(DB.Value Text, DB.Value Money, DB.Value Text)]
queryProudctList = do
    d <- getDeployment
    runDB $ DB.select $ DB.from $ \(c `DB.LeftOuterJoin` p) -> do
        DB.on (c ^. CategoryId DB.==. p ^. ProductCategory)
        DB.where_ (c ^. CategoryDeployment DB.==. (val d))
        return
            ( p ^. ProductName
            , p ^. ProductPrice
            , c ^. CategoryName
            )

queryCategoryList :: Handler [(DB.Value Text, DB.Value CategoryId)]
queryCategoryList = do
    d <- getDeployment
    runDB $ DB.select $ DB.from $ \c -> do
        DB.where_ (c ^. CategoryDeployment DB.==. (val d))
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
