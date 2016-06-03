module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
