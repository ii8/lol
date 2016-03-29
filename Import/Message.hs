
module Import.Message where

import Import.Base
import Data.ByteString as S
import Data.ByteString.Lazy as L
import Data.Word8 as W8
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html (preEscapedToHtml)

msgKey :: Text
msgKey = "msg"

addMessage :: MonadHandler m => Text -> Html -> m ()
addMessage status msg = do
    v <- lookupSessionBS msgKey
    setSessionBS msgKey $ addMsg v
  where
    addMsg = maybe msg' (S.append msg' . S.cons W8._nul)
    msg' = S.append
        (encodeUtf8 status)
        (W8._nul `S.cons` (L.toStrict $ renderHtml msg))

addMessageI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
            => Text -> msg -> m ()
addMessageI status msg = do
    mr <- getMessageRender
    addMessage status $ toHtml $ mr msg

getMessages :: MonadHandler m => m [(Text, Html)]
getMessages = do
    bs <- lookupSessionBS msgKey
    let ms = maybe [] enlist bs
    deleteSession msgKey
    return ms
  where
    enlist = pairup . S.split W8._nul
    pairup [] = []
    pairup [_] = []
    pairup (s:v:xs) = (decodeUtf8 s, preEscapedToHtml (decodeUtf8 v)) : pairup xs

msgAvailable, msgPrice, msgCheap :: IsString a => a
msgAvailable = "pa"
msgPrice = "pp"
msgCheap = "pc"

putm :: Html -> Html
putm msg
    | m == msgAvailable = "Sorry, some products you selected are no longer available. Please try again."
    | m == msgPrice = "Sorry, prices have changed since you made your choice. Please review your selection"
    | m == msgCheap = "Your order must be at least 20p"
    | otherwise = msg
  where
    m = renderHtml msg

puts :: Text -> Text
puts "success" = "alert-success"
puts "info" = "alert-info"
puts "warn" = "alert-warning"
puts "error" = "alert-danger"
puts a = a
