
module Import.Piece (renderPiece) where

import Import.Base
import Import.Enum
import Model
import Foundation
import Database.Persist.Sql (toSqlKey, fromSqlKey)

-- To add a piece template, add hamlet file to templates/pieces
-- and add a new pattern to this function. The third parameter is the
-- name of the piece template. Call `getData' with parents, the current
-- piece id and the key of the data value you want. It will return a
-- widget ready to be embeded in hamlet.
renderPiece' :: [PieceId] -> PieceId -> Text -> Widget
renderPiece' parents key "test" = do
    domainName <- runInputGet $ ireq textField "domain"
    lolDep <- handlerToWidget getDeployment
    nutherDep <- handlerToWidget getDeployment
    let mainBox = getData parents key "mainbox"
    $(widgetFile "pieces/test")
renderPiece' ps key "laxus" = do
    let content = getData ps key "content"
    $(widgetFile "pieces/laxus")
renderPiece' ps key "sinister" = do
    let sidebar = getData ps key "sidebar"
    let content = getData ps key "content"
    $(widgetFile "pieces/sinister")
renderPiece' _ _ _ = toWidget [hamlet|Bad piece template|]

getData :: [PieceId] -> PieceId -> Text -> Widget
getData parents pid key = do
    pdata <- handlerToWidget $ queryData pid key
    renderData parents pdata

queryData :: PieceId -> Text -> Handler (PieceDataType, Text)
queryData piece key = do
    pd <- runDB $ getBy $ UniquePieceData piece key
    case pd of
        Just (Entity _ (PieceData _ t _ v)) -> return (t, v)
        Nothing -> return (Plain, "Missing piece data")

renderData :: [PieceId] -> (PieceDataType, Text) -> Widget
renderData _ (Plain, v) = toWidget [hamlet|#{v}|]
renderData parents (Reference, v) = maybe
    (toWidget [hamlet|Bad piece data value|])
    (renderPiece parents . toSqlKey . fromIntegral)
    (parseInt v)
renderData _ (Markup, v) = toWidget [hamlet|#{v}|]
renderData _ (Link, v) = toWidget [hamlet|#{v}|]

renderPiece :: [PieceId] -> PieceId -> Widget
renderPiece parents key = if key `elem` parents
    then toWidget [hamlet|Recursive piece references detected|]
    else do
        piece <- handlerToWidget . runDB $ get key
        case piece of
            Just (Piece name) -> renderPiece' (key:parents) key name
            Nothing -> toWidget [hamlet|Missing piece|]
