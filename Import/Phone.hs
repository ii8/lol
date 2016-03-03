
module Import.Phone where

import Import.Base
import Text.Julius (rawJS)
import Data.Char (isDigit)
import Text.Blaze (ToMarkup(..))
import Data.Aeson (withText)
import qualified Data.Attoparsec.Text as AP

newtype Phone = Phone Text

instance PersistField Phone where
    toPersistValue (Phone s) = PersistText $ s
    fromPersistValue s = case fromPersistValueText s of
        Right p -> Right $ Phone p
        Left p -> Left p

instance PersistFieldSql Phone where
    sqlType _ = SqlString

instance ToMarkup Phone where
    toMarkup (Phone n) = toMarkup n

instance FromJSON Phone where
    parseJSON = withText "Phone number" $ return . Phone

instance ToJSON Phone where
    toJSON (Phone a) = String a

parsePhone' :: Text -> Either FormMessage Phone
parsePhone' s = let n = filter (\c -> isDigit c) s in
    case apresult $ AP.parse p n of
        Just a -> Right $ Phone a
        Nothing -> Left $ MsgInvalidNumber "Please enter a valid phone number"
  where
    p :: AP.Parser Text
    p = do
        _ <- AP.choice [
            AP.option "" (AP.string "0044"),
            AP.option "" (AP.string "44") ]
        _ <- AP.char '0'
        n <- AP.choice [
            AP.count 10 AP.digit,
            AP.count 9 AP.digit,
            AP.count 7 AP.digit ]
        return $ "0" <> pack n

phoneField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Phone
phoneField = Field { fieldParse = parse, fieldView = view, fieldEnctype = UrlEncoded }
  where
    parse = parseHelper parsePhone'
    view myid name attrs v r = do
        toWidget [hamlet|
<input type="text" id=#{myid} name=#{name} *{attrs} :r:required value="#{showVal v}">
|]
        toWidget [julius|
$("##{rawJS myid}").on('change', function()
{

});
|]
    showVal (Left s) = s
    showVal (Right (Phone s)) = s
