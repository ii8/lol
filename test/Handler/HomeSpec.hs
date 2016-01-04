module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "loads the index and checks it looks right" $ do
        request $ do
            addRequestHeader ("host", "localhost")
            setMethod "GET"
            setUrl HomeR
        statusIs 200
