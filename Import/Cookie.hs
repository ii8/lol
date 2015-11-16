
module Import.Cookie (renderOrder, parseOrder) where

import Import.Base hiding (Value)
import Model
import Database.Persist.Sql (fromSqlKey)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson.Types (Value, Parser, parseMaybe, withObject, withArray)
import Data.Aeson.Parser (json)
import Data.Attoparsec.ByteString (parse, IResult(Done))

newtype OrderCookie = OrderCookie [(ProductId, Int)]

instance ToJSON (ProductId, Int) where
    toJSON (p, q) = object [("product" .= (fromSqlKey p)), ("quantity" .= q)]

instance ToJSON OrderCookie where
    toJSON (OrderCookie a) = array a

parseOrderTuple :: Value -> Parser (ProductId, Int)
parseOrderTuple = withObject "order tuple" $ \o -> (,) <$> o .: "product" <*> o .: "quantity"

instance FromJSON OrderCookie where
    parseJSON = withArray "order array" $ \a -> OrderCookie <$> mapM parseOrderTuple (toList a)

parseOrder :: Text -> Maybe OrderCookie
parseOrder s = let bs = encodeUtf8 s in
    case parse json bs of
        (Done _ r) -> parseMaybe parseJSON r
        _ -> Nothing

renderOrder :: OrderCookie -> Text
renderOrder = toStrict . toLazyText . encodeToTextBuilder . toJSON

