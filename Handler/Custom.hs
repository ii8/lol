
module Handler.Custom (getHomeR, getCustomR) where

import Import

getCustomR :: Text -> Handler Html
getCustomR name = do
    domain <- runInputGet $ ireq textField "domain"
    (Entity deployment _) <- runDB $ getBy404 $ UniqueDomain domain
    (Entity _ (Page _ _ piece)) <- runDB $ getBy404 $ UniquePage deployment name
    defaultLayout $ setTitle (toHtml name) >> renderPiece piece

getHomeR :: Handler Html
getHomeR = getCustomR "home"
