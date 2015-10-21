module Handler.Menu where

import Import

getMenuR :: Handler Html
getMenuR = defaultLayout $(widgetFile "menu")
