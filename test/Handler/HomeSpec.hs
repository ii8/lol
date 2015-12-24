module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "loads the index and checks it looks right" $ do
        request $ do
            setMethod "GET"
            setUrl HomeR
            addGetParam "domain" "localhost"
        statusIs 200
