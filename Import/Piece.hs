
module Import.Piece (renderPiece) where

import Import.Base
import Model
import Foundation

renderPiece' :: Key Piece -> Text -> Widget
renderPiece' _ "test" = do
    domainName <- runInputGet $ ireq textField "domain"
    $(widgetFile "pieces/test")
renderPiece' _ _ = notFound

renderPiece :: Key Piece -> Widget
renderPiece key = do
    Piece name <- handlerToWidget . runDB $ get404 key
    renderPiece' key name
