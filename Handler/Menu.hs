module Handler.Menu where

import Import
import Database.Persist.Sql (fromSqlKey)

getMenuR :: Handler Html
getMenuR = do
    rows <- runDB query
    defaultLayout $ do
        $(widgetFile "menu")

query = do
    cats <- selectList [] [Asc CategoryOrder]
    forM cats $ \(Entity catId cat) -> do
        products <- selectList
            [ProductCategory ==. catId]
            [Asc ProductName]
        return (cat, products)
