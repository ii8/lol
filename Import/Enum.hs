
module Import.Enum where

import Import.Base
import Data.Aeson (withText)
import Database.Persist.TH (derivePersistField)

data UserType = Customer | Manager | Admin
    deriving (Show, Read, Eq, Ord)

derivePersistField "UserType"

data PieceDataType = Plain | Reference | Markup
    deriving (Show, Read, Eq)

derivePersistField "PieceDataType"

data OrderStatus =  New | Complete | Cancelled
    deriving (Show, Read, Eq, Enum, Bounded)

instance FromJSON OrderStatus where
    parseJSON = withText "Order Status" $ return . read

instance ToJSON OrderStatus where
    toJSON = String . show

derivePersistField "OrderStatus"

data PaymentStatus = Payable | Paid | Unpaid | Refunded
    deriving (Show, Read, Eq, Enum, Bounded)

instance FromJSON PaymentStatus where
    parseJSON = withText "Payment Status" $ return . read

instance ToJSON PaymentStatus where
    toJSON = String . show

derivePersistField "PaymentStatus"

