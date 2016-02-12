
module Handler.Page where

import Import
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Network.HTTP.Types.Status (conflict409)

queryPages :: Handler [(Entity Page)]
queryPages = do
    d <- getDeploymentId
    runDB $ select $ from $ \p -> do
        where_ $ p ^. PageDeployment ==. val d
        return p

form :: Form Text
form extra = do
    (result, view) <- mreq textField "" Nothing
    let widget = toWidget [whamlet|
<div>
  #{extra}
  ^{fvInput view}
  <input type="submit" class="btn btn-green" value="New Page">
|]
    return (result, widget)

getPageR :: Handler Html
getPageR = do
    ps <- queryPages
    (widget, enc) <- generateFormPost form
    defaultLayout $(widgetFile "page")

postPageNewR :: Handler Html
postPageNewR = do
    ((result, widget), enc) <- runFormPost form
    case result of
        FormSuccess name -> newPage name >>= redirect . PageEditR
        _ -> queryPages >>= \ps -> defaultLayout $(widgetFile "page")
  where
    newPage name = do
        d <- getDeploymentId
        piece <- runDB $ insert $ Piece d "default"
        runDB $ insert $ Page d name piece

getPageEditR :: PageId -> Handler Html
getPageEditR pid = do
    page <- queryPage
    defaultLayout $ do
        $(widgetFile "page-edit")
        renderPiece $ pagePiece page
  where
    queryPage :: Handler Page
    queryPage = do
        d <- getDeploymentId
        p <- runDB $ select $ from $ \p -> do
            where_ $ p ^. PageId ==. val pid &&. p ^. PageDeployment ==. val d
            return p
        case p of
            ((Entity _ page):[]) -> return page
            _ -> notFound

postAjaxPagePieceR :: PieceId -> Handler Json.Value
postAjaxPagePieceR pid = do
    template <- getJson $ \o -> o .: "template"

    d <- getDeploymentId
    runDB $ update $ \p -> do
        set p [ PieceTemplate =. val template ]
        where_ $ p ^. PieceId ==. val pid &&. p ^. PieceDeployment ==. val d

    return Json.Null

deleteAjaxPagePieceR :: PieceId -> Handler Json.Value
deleteAjaxPagePieceR pid = do
    r <- runDB $ select $ from $ \(piece `LeftOuterJoin` page) -> do
        on $ just (piece ^. PieceId) ==. page ?. PagePiece
        where_ $ piece ^. PieceId ==. val pid
        return ( piece ^. PieceDeployment, page ?. PageId )

    case r of
        ((Value pd, Value mp):_) -> do
            d <- getDeploymentId
            when (isJust mp) (sendResponseStatus conflict409 ())
            when (pd /= d) notFound
        _ -> notFound

    runDB $ do
        delete $ from $ \p -> do
            where_ $ p ^. PieceDataPiece ==. val pid
        delete $ from $ \p -> do
            where_ $ p ^. PieceId ==. val pid

    return Json.Null

postAjaxPageDataR :: PieceId -> Text -> Handler Json.Value
postAjaxPageDataR pid key = do
    (mt, v) <- getJson $ \o -> do
        t <- o .: "type"
        v <- o .: "value"
        return (readMay t, v)
    t <- maybe (invalidArgs []) return mt

    d <- getDeploymentId
    r <- runDB $ select $ from $ \p -> do
        where_ $ p ^. PieceId ==. val pid
        return ( p ^. PieceDeployment )
    case r of
        ((Value pd):_) -> if pd /= d
            then notFound
            else return ()
        [] -> notFound

    runDB $ update $ \p -> do
        set p [ PieceDataType =. val t, PieceDataValue =. val v ]
        where_ $ p ^. PieceDataPiece ==. val pid
            &&. p ^. PieceDataKey ==. val key

    -- PieceData might actually use special css etc...
    return Json.Null


getJson :: FromJSON a => (a -> Json.Parser b) -> Handler b
getJson p = do
    body <- requireJsonBody
    let m = flip Json.parseMaybe body p
    maybe (invalidArgs []) return m
