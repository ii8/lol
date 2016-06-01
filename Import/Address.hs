{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import.Address where

import Import.Base
import Text.Blaze.Html5
import Model

instance ToMarkup Address where
    toMarkup addr = do
        maybe (return ()) (\x -> text x >> br) (addressName addr)
        text (addressLineone addr) >> br
        maybe (return ()) (\x -> text x >> br) (addressLinetwo addr)
        text (addressTown addr) >> br
        text (addressCounty addr) >> br
        text $ addressPostcode addr
