
module Handler.Custom (getHomeR, getCustomR) where

import Import

runHandler :: Text -> Key Page -> Handler Html
runHandler "default" page = do
    defaultLayout $ do
        domainName <- runInputGet $ ireq textField "domain"
        $(widgetFile "default/homepage")
runHandler _ _ = notFound

getCustomR :: Text -> Handler Html
getCustomR page = do
    domain <- runInputGet $ ireq textField "domain"
    (Entity deployment _) <- runDB $ getBy404 $ UniqueDomain domain
    (Entity pageId (Page _ _ title template)) <- runDB $ getBy404 $ UniquePage deployment page
    runHandler template pageId

getHomeR :: Handler Html
getHomeR = getCustomR "home"
