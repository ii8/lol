
module Import.Money (Money(..), moneyField) where

import ClassyPrelude.Yesod
import Data.Char (isDigit)
import Text.Read (read)
import Data.List (tail)
import qualified Data.Text as T
import Database.Persist.Sql (PersistFieldSql(..))
import Text.Blaze (ToMarkup(..))

newtype Money = Money Int

instance Num Money where
    (+) (Money a) (Money b) = Money (a + b)
    (*) (Money a) (Money b) = Money (a * b)
    abs (Money a) = Money (abs a)
    signum (Money a) = Money (signum a)
    fromInteger a = Money (fromInteger a * 100)
    negate (Money a) = Money (negate a)

readMoney :: String -> Money
readMoney a = Money $ fst $ (read a :: (Int, String))

dot :: Money -> Text
dot (Money m) = let s = show m in pack $ cat $ splitAt (length s - 2) s
  where cat (a, b) = a ++ "." ++ b

instance PersistField Money where
    toPersistValue (Money i) = PersistInt64 $ fromIntegral i
    fromPersistValue (PersistInt64 i) = Right $ Money $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right $ Money (truncate i :: Int) -- oracle, what a shit database
    fromPersistValue x = Left $ T.pack $ "int Expected Integer, received: " ++ show x

instance PersistFieldSql Money where
    sqlType _ = SqlInt64

instance ToMarkup Money where
    toMarkup a = "£" ++ (toMarkup (dot a))

moneyField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Money
moneyField = Field { fieldParse = parse, fieldView = view, fieldEnctype = UrlEncoded }
  where
    parse = parseHelper $ \s -> case (\(a, b) -> (unpack a, tail $ unpack b)) $ T.breakOn "." s of
        (xs, [x, y]) -> if all isDigit (x:y:xs)
            then Right $ readMoney xs * 100 + readMoney [x, y]
            else Left err
        (xs, []) -> if all isDigit xs
            then Right $ readMoney xs * 100
            else Left err
        _ -> Left err
    view myid name attrs val req = toWidget [whamlet|
<input type="number" id=#{myid} name=#{name} *{attrs} :req:required value="#{showVal val}">
|]
    showVal = either id dot
    err = MsgInvalidEntry "Invalid monetary value"

