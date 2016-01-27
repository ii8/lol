
module Handler.Page where

import Import

getPageR :: Handler Html
getPageR = notFound

getPageNewR :: Handler Html
getPageNewR = notFound

postPageNewR :: Handler Html
postPageNewR = notFound

getPageEditR :: PageId -> Handler Html
getPageEditR _ = notFound

postPageEditR :: PageId -> Handler Html
postPageEditR _ = notFound
