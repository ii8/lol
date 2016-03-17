
module Handler.Page where

import Import
import qualified Data.Aeson as Json
import Data.Text as Text (cons, foldr)
import Data.Char (isDigit, isLetter, toLower)
import Network.HTTP.Types.Status (conflict409)

queryPages :: Handler [(Entity Page)]
queryPages = do
    d <- getDeploymentId
    runDB $ select $ from $ \p -> do
        where_ $ p ^. PageDeployment ==. val d
        return p

form :: Form Text
form extra = do
    (result, view) <- mreq pageName "" Nothing
    let widget = toWidget [whamlet|
<div>
  #{extra}
  ^{fvInput view}
  <input type="submit" class="btn btn-green" value="New Page">
  $maybe err <- fvErrors view
    #{err}
|]
    return (result, widget)
  where
    pageName = check'em validate textField
    validate s = do
        let s' = slug s
        d <- getDeploymentId
        m <- runDB $ checkUnique $ Page d s' (toSqlKey 0)
        return $ case m of
            Just _ -> Left ("Already exists" :: Text)
            Nothing -> Right s'
    check'em = checkM
    slug str = snd $ Text.foldr (\c (b, s) -> new b s c) (False, "") str
    new b s c =
        let r = rep c
            b' = r == '_'
        in (b', if b && b' then s else r `cons` s)
    rep c
        | isDigit c = c
        | isLetter c = toLower c
        | otherwise = '_'


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

    -- If linking to another piece, check it's ours.
    when (t == Reference) $ do
        ref <- maybe
            (invalidArgs [])
            (return . toSqlKey . fromIntegral)
            (parseUnsigned v)
        r <- runDB $ select $ from $ \p -> do
            where_ $ p ^. PieceId ==. val ref
            return ( p ^. PieceDeployment )
        case r of
            ((Value rd):_) -> if (rd /= d)
                then invalidArgs ["Reference piece does not exist"]
                else return ()
            _ -> invalidArgs ["Reference piece does not exist"]

    r <- runDB $ select $ from $ \p -> do
        where_ $ p ^. PieceId ==. val pid
        return ( p ^. PieceDeployment )
    case r of
        ((Value pd):_) -> do
            when (pd /= d) notFound
            c <- getCount
            case c of
                ((Value 0):_) -> insertNew t v
                _ -> updateExisting t v
        [] -> notFound

    -- PieceData might actually use special css etc...
    return Json.Null
  where
    getCount :: Handler [Value Int]
    getCount = runDB $ select $ from $ \p -> do
        where_ $ p ^. PieceDataPiece ==. val pid
            &&. p ^. PieceDataKey ==. val key
        return countRows
    insertNew t v = void $ runDB $ insert $ PieceData pid t key v
    updateExisting t v = runDB $ update $ \p -> do
        set p [ PieceDataType =. val t, PieceDataValue =. val v ]
        where_ $ p ^. PieceDataPiece ==. val pid
            &&. p ^. PieceDataKey ==. val key

