module Handler.Menu where

import Import
<<<<<<< HEAD
=======
import Data.Ord (comparing)
import Database.Persist.Sql (fromSqlKey)
>>>>>>> Some more menu changes and handler change thingy that makes the product id thingy work.

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
