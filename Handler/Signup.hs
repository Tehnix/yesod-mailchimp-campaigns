module Handler.Signup where

import Import

import Handler.SignupForm (signupForm)
import Handler.Widgets    (languageSelectorWidget, signupBannerWidget)

-- | The signup page without any referrer.
getSignupR :: Handler Html
getSignupR = redirect $ SignupIR defaultLanguage

getSignupIR :: Language -> Handler Html
getSignupIR lang = do
  messageRender <- getMessageRender
  mmsg <- getMessage
  master <- getYesod
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm
                                             master lang messageRender Nothing
  setUltDestCurrent
  internationalLayout lang $ do
    setTitleI MsgSignupTitle
    $(widgetFile "homepage")
  where
    route = SignupIR
