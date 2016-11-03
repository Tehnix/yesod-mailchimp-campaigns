module Internationalization.Types where

import ClassyPrelude.Yesod

data Language = Danish | Swedish | Norwegian
  deriving (Show, Read, Eq)
derivePersistField "Language"

-- | Defines how the URL will look for each language
instance PathPiece Language where
  fromPathPiece "da" = Just Danish
  fromPathPiece "se" = Just Swedish
  fromPathPiece "no" = Just Norwegian
  fromPathPiece _    = Nothing

  toPathPiece Danish    = "da"
  toPathPiece Swedish   = "se"
  toPathPiece Norwegian = "no"

-- | Convenience function for setting a language using the Language datatype
setLanguage' :: MonadHandler m => Language -> m ()
setLanguage' lang = case lang of
  Danish    -> setLanguage "da"
  Swedish   -> setLanguage "sv"
  Norwegian -> setLanguage "nb"

-- | The default language to fall back to
defaultLanguage :: Language
defaultLanguage = Danish
