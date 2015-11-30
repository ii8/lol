
module Import.Enum where

import Import.Base
import Database.Persist.TH (derivePersistField)

data UserType = Customer | Manager | Admin
    deriving (Show, Read, Eq, Ord)

derivePersistField "UserType"

data PieceDataType = Plain | Reference | Markup
    deriving (Show, Read, Eq)

derivePersistField "PieceDataType"

