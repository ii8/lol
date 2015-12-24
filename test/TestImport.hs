{-# LANGUAGE NoCPP #-}

module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return foundation

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask
    let queries = map (\t -> "TRUNCATE TABLE " ++ connEscapeName sqlBackend (DBName t)) tables

    -- In MySQL, a table cannot be truncated if another table references it via foreign key.
    -- Since we're wiping both the parent and child tables, though, it's safe
    -- to temporarily disable this check.
    rawExecute "SET foreign_key_checks = 0;" []
    forM_ queries (\q -> rawExecute q [])
    rawExecute "SET foreign_key_checks = 1;" []

    rawExecute "insert into `deployment` set \
               \`name` = 'test',\
               \`domain` = 'localhost',\
               \`wrapper` = 'navbar',\
               \`stripe_public` = 'pk_test_wUAzZy675JJKqVCFMU2FcSBB',\
               \`stripe_secret` = 'sk_test_cC82zVtq4YT0Y90ZuXesESTX';" []
    rawExecute "insert into `piece` set \
               \`template` = 'test';" []
    rawExecute "insert into `page` set \
               \`deployment` = (select `id` from `deployment` where `domain` = 'localhost'),\
               \`name` = 'home',\
               \`piece` = 1;" []
    rawExecute "insert into `piece_data` set \
               \`piece` = 1,\
               \`key` = 'mainbox',\
               \`value` = 'Hello I am custom page data',\
               \`type` = 'Plain';" []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SHOW TABLES;" []
    return $ map unSingle tables
