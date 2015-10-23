
module Handler.Order where

import Import


getOrderR :: Handler Html
getOrderR = do
    cookie <- lookupCookie "order"
    let derp = parseOrder =<< cookie
    let lolvar = liftM renderOrder derp
    defaultLayout $ do
        $(widgetFile "order")

postOrderR :: Handler Html
postOrderR = notFound

