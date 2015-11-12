
module Import.Piece (renderPiece) where

import Import.Base
import Model
import Foundation
import Database.Persist.Sql (fromSqlKey)

renderPiece' :: Key Piece -> Text -> Widget
renderPiece' _ "test" = do
    domainName <- runInputGet $ ireq textField "domain"
    lolDep <- handlerToWidget getDeployment
    nutherDep <- handlerToWidget getDeployment
    $(widgetFile "pieces/test")
renderPiece' _ _ = notFound

renderPiece :: Key Piece -> Widget
renderPiece key = do
    Piece name <- handlerToWidget . runDB $ get404 key
    renderPiece' key name
