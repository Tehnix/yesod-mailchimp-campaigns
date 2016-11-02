module Internationalization.Types where

import ClassyPrelude.Yesod

data LanguageSelection = LanguageSelection
  { selectedLanguage :: Language
  } deriving Show

data Language = Danish | Swedish | Norwegian
  deriving (Show, Read, Eq)
derivePersistField "Language"

instance PathPiece Language where
  fromPathPiece "da" = Just Danish
  fromPathPiece "se" = Just Swedish
  fromPathPiece "no" = Just Norwegian
  fromPathPiece _    = Nothing

  toPathPiece Danish    = "da"
  toPathPiece Swedish   = "se"
  toPathPiece Norwegian = "no"

-- MonadHandler m => Language -> m ()
-- setLanguage' :: Language -> Handler
setLanguage' lang = case lang of
  Danish    -> setLanguage "da"
  Swedish   -> setLanguage "sv"
  Norwegian -> setLanguage "nb"

-- | The default language to fall back to
defaultLanguage :: Language
defaultLanguage = Danish
