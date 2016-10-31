module Handler.Signup where
import           Import

import           Handler.SignupForm (signupForm)
import           Handler.Widgets    (languageSelectorWidget, signupBannerWidget)


-- | The signup page without any referrer
getSignupR :: Handler Html
getSignupR = redirect $ SignupIR defaultLanguage

getSignupIR :: Language -> Handler Html
getSignupIR lang = do
  setLanguage' lang
  messageRender <- getMessageRender
  mmsg <- getMessage
  let route = SignupIR
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm lang messageRender Nothing
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  defaultLayout $ do
    setTitleI MsgSignupTitle
    $(widgetFile "homepage")
