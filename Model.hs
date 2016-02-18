module Model where

import BasicPrelude
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH

import Import.Money
import Import.Phone
import Import.Enum

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
