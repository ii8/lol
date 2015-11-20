
module Import.Enum where

import Import.Base

data UserType = Customer | Manager | Admin
    deriving (Show, Read, Eq, Ord)

derivePersistField "UserType"

data PieceDataType = Plain | Reference | Markup | Link
    deriving (Show, Read, Eq)

derivePersistField "PieceDataType"

