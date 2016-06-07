module Handler.Menu where

import Import

query :: Handler [(Text, [(Int64, Text, Money)])]
query = do
    d <- getDeploymentId
    rows <- runDB $ select $ from $ \(c `InnerJoin` p) -> do
        on (c ^. CategoryId ==. p ^. ProductCategory)
        where_ (c ^. CategoryDeployment ==. (val d) &&. p ^. ProductAvailable)
        return
            ( c ^. CategoryName
            , p ^. ProductId
            , p ^. ProductName
            , p ^. ProductPrice
            )
    return $ foldr categorise [] rows
  where
    categorise (Value cat, Value key, Value name, Value price) (x@(current, sublist):xs) =
        if cat == current
            then (cat, (fromSqlKey key, name, price):sublist):xs
            else (cat, [(fromSqlKey key, name, price)]):x:xs
    categorise (Value cat, Value key, Value name, Value price) [] =
        [(cat, [(fromSqlKey key, name, price)])]

getMenuR :: Handler Html
getMenuR = do
    rows <- query
    defaultLayout $ do
        setTitle "Menu"
        addScript $ StaticR global_js_order_js
        $(widgetFile "menu")
