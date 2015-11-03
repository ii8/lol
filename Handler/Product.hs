
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
    ps <- runDB $ selectList [] [Asc ProductPrice]
    defaultLayout $(widgetFile "product-list")

getProductNewR :: Handler Html
getProductNewR = do
    ((result, widget), enc) <- runFormPost form
    defaultLayout $(widgetFile "product-edit")


postProductNewR :: Handler Html
postProductNewR = getProductNewR

getProductEditR :: Key Product -> Handler Html
getProductEditR p = notFound

postProductEditR :: Key Product -> Handler Html
postProductEditR p = notFound
