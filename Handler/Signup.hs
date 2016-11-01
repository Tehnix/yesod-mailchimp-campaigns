module Handler.Signup where
import           Import

import           Handler.SignupForm (signupForm)
import           Handler.Widgets    (languageSelectorWidget, signupBannerWidget)


-- | The signup page without any referrer
getSignupR :: Handler Html
getSignupR = redirect $ SignupIR defaultLanguage

getSignupIR :: Language -> Handler Html
getSignupIR lang = do
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  setLanguage' lang
  messageRender <- getMessageRender
  mmsg <- getMessage
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm lang messageRender Nothing
  let route = SignupIR
  internationalLayout lang $ do
    setTitleI MsgSignupTitle
    $(widgetFile "homepage")
