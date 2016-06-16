
module Import.Piece (renderPiece, renderData) where

import Import.Base
import Import.Enum
import Model
import Foundation
import Text.Markdown
import Data.Text (splitOn)
import Text.Julius (rawJS)

-- To add a piece template, add hamlet file to templates/pieces
-- and add a new pattern to this function. The third parameter is the
-- name of the piece template. Call `getData' with parents, the current
-- piece id and the key of the data value you want. It will return a
-- widget ready to be embeded in hamlet.
renderPiece' :: [PieceId] -> PieceId -> Text -> Widget
renderPiece' ps key template =
    (\widget -> [whamlet|
<div data-piece-id="#{fromSqlKey key}">
  ^{widget}
|]) $ case template of
    "default" -> do
        $(widgetFile "pieces/default")
    "test" -> do
        domain <- handlerToWidget $ deploymentDomain <$> getDeployment
        lolDep <- handlerToWidget getDeploymentId
        nutherDep <- handlerToWidget getDeploymentId
        req <- return . show =<< waiRequest
        let mainBox = getData ps key "mainbox"
        $(widgetFile "pieces/test")
    "laxus" -> do
        let content = getData ps key "content"
        $(widgetFile "pieces/laxus")
    "sinister" -> do
        let sidebar = getData ps key "sidebar"
        let content = getData ps key "content"
        $(widgetFile "pieces/sinister")
    "rizon" -> do
        (t, channel) <- handlerToWidget $ queryData key "channel"
        let alpha = (==) 0 $ length $ filter (flip notElem ['a'..'z']) channel
        if t /= Plain || not alpha
            then toWidget [hamlet|Rizon piece: error: bad channel|]
            else $(widgetFile "pieces/rizon")
    "scroll" -> do
        let text = getData ps key "text"
        let cssClass = "scrolling-text-" <> (show $ fromSqlKey key)
        $(widgetFile "pieces/scroll")
    "bigbanner" -> do
        domain <- handlerToWidget $ deploymentDomain <$> getDeployment
        $(widgetFile "pieces/bigbanner")
    "image" -> do
        domain <- handlerToWidget $ deploymentDomain <$> getDeployment
        (_, image) <- handlerToWidget $ queryData key "image"
        $(widgetFile "pieces/image")
    "imgmenu" -> do
        domain <- handlerToWidget $ getDomain
        (_, raw) <- handlerToWidget $ queryData key "links"
        let list = splitOn ";" raw
            item (a:b:c:[]) = ( a, b, local domain (splitOn "/" c) )
            item _ = error "Invalid list"
            links = fmap (item . splitOn ",") list
            pid = rawJS . show $ fromSqlKey key
        addScript $ StaticR global_js_imgmenu_js
        $(widgetFile "pieces/imgmenu")
    _ -> toWidget [hamlet|Bad piece template|]

getData :: [PieceId] -> PieceId -> Text -> Widget
getData parents pid key = do
    pdata <- handlerToWidget $ queryData pid key
    let widget = renderData' parents pdata
    [whamlet|<div data-piece-data="#{key}">^{widget}|]

queryData :: PieceId -> Text -> Handler (PieceDataType, Text)
queryData piece key = do
    pd <- runDB $ getBy $ UniquePieceData piece key
    case pd of
        Just (Entity _ (PieceData _ t _ v)) -> return (t, v)
        Nothing -> return (Plain, "Missing piece data")

renderData' :: [PieceId] -> (PieceDataType, Text) -> Widget
renderData' _ (Plain, v) = toWidget [hamlet|#{v}|]
renderData' parents (Reference, v) = maybe
    (toWidget [hamlet|Bad piece data value|])
    (renderPiece'' parents . toSqlKey . fromIntegral)
    (parseInt v)
renderData' _ (Markup, v) = toWidget [hamlet|#{markdown def (fromStrict v)}|]
renderData' _ (LinkList, _) = toWidget [hamlet|Invalid link list|]

renderPiece'' :: [PieceId] -> PieceId -> Widget
renderPiece'' parents key = if key `elem` parents
    then toWidget [hamlet|Recursive piece references detected|]
    else do
        piece <- handlerToWidget . runDB $ get key
        case piece of
            Just (Piece _ name) -> renderPiece' (key:parents) key name
            Nothing -> toWidget [hamlet|Missing piece|]

renderPiece :: PieceId -> Widget
renderPiece = renderPiece'' []

renderData :: PieceDataType -> Text -> Widget
renderData t d = renderData' [] (t, d)
