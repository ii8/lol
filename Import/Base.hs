module Import.Base
    ( module Import
    , apresult
    , parseInt
    , parseInt'
    ) where

import BasicPrelude          as Import hiding (on, groupBy, insert, insertBy, delete,
                                               deleteBy, length, splitAt, filter)
import Data.Text             as Import (pack, unpack, length, splitAt, filter)
import Data.Text.Lazy        as Import (toStrict, fromStrict)
import Yesod.Core            as Import hiding (Header, Value)
import Yesod.Form            as Import hiding (parseTime)
import Yesod.Static          as Import
import Yesod.Persist.Core    as Import
import Network.HTTP.Client.Conduit as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Database.Esqueleto    as Import hiding (isNothing)
import Data.Default          as Import

import qualified Data.Attoparsec.Text as AP

apresult :: AP.Result r -> Maybe r
apresult (AP.Fail _ _ _) = Nothing
apresult (AP.Partial c) = apresult (c "")
apresult (AP.Done _ r) = Just r

parseInt :: Text -> Maybe Int
parseInt str = apresult $ AP.parse (AP.signed AP.decimal) str

-- Unsafe Int read
parseInt' :: Text -> Int
parseInt' str = case parseInt str of
    Just a -> a
    Nothing -> error $ "unsafe use of parseInt on: " ++ (unpack str)
