
module Import.Layout (layout) where

import Import.Base
import Import.Enum
import Import.Piece
import Foundation
import Model
import Text.Julius (juliusFile)
import qualified Data.Aeson as Json
import qualified Data.CaseInsensitive as CI

import Import.Message

wrap :: Widget -> Text -> Widget
wrap w "navbar" = do
    maid <- handlerToWidget maybeAuthId
    ms <- getMessages
    manager <- handlerToWidget $ checkLevel Manager
    domain <- handlerToWidget getDomain
    r <- handlerToWidget getCurrentRoute
    wd <- handlerToWidget wrapData
    let isHome = case r of
            Just HomeR -> True
            Just (CustomR "home") -> True
            _ -> False
        mhome' = wrapKey "home" wd
        mhome = fmap (\(_, t, v) -> renderData t v) mhome'
    $(widgetFile "wrappers/navbar")
wrap w _ = getMessages >>= (\ms -> $(widgetFile "wrappers/default-layout"))

wrapData :: Handler [(Value Text, Value PieceDataType, Value Text)]
wrapData = do
    d <- getDeploymentId
    runDB $ select $ from $ \wd -> do
        where_ $ wd ^. WrapDataDeployment ==. val d
        return ( wd ^. WrapDataKey, wd ^. WrapDataType, wd ^. WrapDataValue )

wrapKey :: Text -> [(Value Text, Value PieceDataType, Value Text)] -> Maybe (Text, PieceDataType, Text)
wrapKey _ [] = Nothing
wrapKey key (r@(Value k, _, _):vs)
    | k == key = Just $ unValue3 r
    | otherwise = wrapKey key vs

wrapType :: Text
         -> PieceDataType
         -> [(Value Text, Value PieceDataType, Value Text)]
         -> Maybe (Text, PieceDataType, Text)
wrapType key t l = wrapKey key l >>= (\r@(_, t', _) -> if t == t' then Just r else Nothing)

layout :: WidgetT App IO () -> HandlerT App IO Html
layout widget = do
    -- Need getDeploymentSafe because the subwidget might be a 404 response already.
    (domain, wrapper) <- do
        md <- getDeploymentSafe
        return $ case md of
            Just d -> (deploymentDomain d, deploymentWrapper d)
            Nothing -> ("fallback", "")

    token <- returnJson . reqToken =<< getRequest
    let csrfHeader = Json.toJSON $ decodeUtf8 $ CI.original defaultCsrfHeaderName

    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR global_css_bootstrap_css
        addStylesheet $ StaticR global_css_base_css
        addStylesheet $ local domain ["css", "style.css"]
        addScript $ StaticR global_js_jquery_1_11_3_min_js
        addScript $ StaticR global_js_bootstrap_min_js
        toWidget $(juliusFile "templates/ajax.julius")
        wrap widget wrapper
    withUrlRenderer [hamlet|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>#{pageTitle pc}
    <meta name="viewport" content="width=device-width, initial-scale=1">
    ^{pageHead pc}
  <body>
    ^{pageBody pc}
|]
