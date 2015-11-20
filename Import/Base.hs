module Import.Base
    ( module Import
    , apresult
    , parseInt
    , parseInt'
    ) where

import ClassyPrelude.Yesod   as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import

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
