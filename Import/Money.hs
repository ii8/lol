
module Import.Money (Money(..), moneyField, parseMoney, renderMoney) where

import Import.Base
import Data.Char (isDigit)
import Text.Blaze (ToMarkup(..))
import qualified Data.Attoparsec.Text as AP

newtype Money = Money Int

instance Num Money where
    (+) (Money a) (Money b) = Money (a + b)
    (*) (Money a) (Money b) = Money (a * b)
    abs (Money a) = Money (abs a)
    signum (Money a) = Money (signum a)
    fromInteger a = Money (fromInteger a * 100)
    negate (Money a) = Money (negate a)

renderMoney :: Money -> Text
renderMoney (Money m) = let s = show m in pack $ cat $ splitAt (length s - 2) s
  where cat (a, b) = a ++ "." ++ b

parseMoney' :: Text -> Either FormMessage Money
parseMoney' s = case apresult $ AP.parse p s of
    Just a -> Right $ Money a
    Nothing -> Left $ MsgInvalidNumber "Please enter a valid monetary value, eg: 12.50"
  where
    p :: AP.Parser Int
    p = do
        a <- AP.takeWhile isDigit
        b <- AP.option ("00" :: Text) q
        _ <- AP.endOfInput
        return $ parseInt' (a <> b)
    q :: AP.Parser Text
    q = do
        _ <- AP.char '.'
        b <- AP.digit
        c <- AP.digit
        return $ pack [b, c]

parseMoney :: Text -> Maybe Money
parseMoney s = case parseMoney' s of
    Right a -> Just a
    Left _ -> Nothing

instance PersistField Money where
    toPersistValue (Money i) = PersistInt64 $ fromIntegral i
    fromPersistValue (PersistInt64 i) = Right $ Money $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right $ Money (truncate i :: Int) -- oracle, what a shit database
    fromPersistValue x = Left $ pack $ "int Expected Integer, received: " ++ show x

instance PersistFieldSql Money where
    sqlType _ = SqlInt64

instance ToMarkup Money where
    toMarkup a = "£" ++ (toMarkup (renderMoney a))

moneyField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Money
moneyField = Field { fieldParse = parse, fieldView = view, fieldEnctype = UrlEncoded }
  where
    parse = parseHelper parseMoney'
    view myid name attrs v req = toWidget [whamlet|
<input type="number" step="0.01" id=#{myid} name=#{name} *{attrs} :req:required value="#{showVal v}">
|]
    showVal = either id renderMoney

