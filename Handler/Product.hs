
module Handler.Product where

import Import
import Database.Persist.Sql (toSqlKey)

form :: Form Product
form = renderDivs $ Product
    <$> pure (toSqlKey 1)
    <*> pure (toSqlKey 1)
    <*> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq moneyField "Price" Nothing

getProductR :: Handler Html
getProductR = do
    deployment <- getDeployment
    ps <- runDB $ selectList [ProductDeployment ==. deployment] [Asc ProductPrice]
    defaultLayout $(widgetFile "product-list")

getProductEditR :: Key Product -> Handler Html
getProductEditR _ = do
    ((result, widget), enc) <- runFormPost form
    defaultLayout $(widgetFile "product-edit")

postProductEditR :: Key Product -> Handler Html
postProductEditR p = notFound
