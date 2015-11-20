
module Import.Piece (renderPiece) where

import Import.Base
import Import.Enum
import Model
import Foundation
import Database.Persist.Sql (toSqlKey, fromSqlKey)

queryPieceData :: PieceId -> Text -> Handler (Int, Text)
queryPieceData piece key = do
    pd <- runDB $ getBy $ UniquePieceData piece key
    case pd of
        Just (Entity _ (PieceData _ t _ v)) -> return (t, v)
        Nothing -> return (dataPlain, "Missing piece data")

renderPiece' :: [PieceId] -> PieceId -> Text -> Widget
renderPiece' parents key "test" = do
    domainName <- runInputGet $ ireq textField "domain"
    lolDep <- handlerToWidget getDeployment
    nutherDep <- handlerToWidget getDeployment
    mainBoxData <- handlerToWidget $ queryPieceData key "mainbox"
    let mainBox = renderData parents mainBoxData
    $(widgetFile "pieces/test")
renderPiece' _ _ _ = toWidget [hamlet|Bad piece template|]

renderData :: [PieceId] -> (Int, Text) -> Widget
renderData parents (t, v)
    | t == dataPlain = toWidget [hamlet|#{v}|]
    | t == dataPiece = maybe
        (toWidget [hamlet|Bad piece data value|])
        (renderPiece parents . toSqlKey . fromIntegral)
        (parseInt v)
    | t == dataMarkup = toWidget [hamlet|#{v}|]
    | t == dataLink = toWidget [hamlet|#{v}|]
    | otherwise = error "Bad piece data type"

renderPiece :: [PieceId] -> PieceId -> Widget
renderPiece parents key = if key `elem` parents
    then toWidget [hamlet|Recursive piece references detected|]
    else do
        piece <- handlerToWidget . runDB $ get key
        case piece of
            Just (Piece name) -> renderPiece' (key:parents) key name
            Nothing -> toWidget [hamlet|Missing piece|]
