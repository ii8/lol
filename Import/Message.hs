
module Import.Message where

import Import.Base
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

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
