module Handler.Language where
import           Import


getSetLanguageR :: Language -> Handler Html
getSetLanguageR lang = do
  setLanguage' lang
  redirectUltDest SignupR
