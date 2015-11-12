
module Handler.Custom (getHomeR, getCustomR) where

import Import

getCustomR :: Text -> Handler Html
getCustomR name = do
    deployment <- getDeployment
    (Entity _ (Page _ _ piece)) <- runDB $ getBy404 $ UniquePage deployment name
    defaultLayout $ setTitle (toHtml name) >> renderPiece piece

getHomeR :: Handler Html
getHomeR = getCustomR "home"
