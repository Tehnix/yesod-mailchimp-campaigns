module Handler.ReferAFriend where
import           Import

import           Handler.SignupForm (signupForm)
import           Handler.Widgets    (languageSelectorWidget, signupBannerWidget)


-- | The signup page with a referrer
getReferAFriendR :: Text -> Handler Html
getReferAFriendR referrer = redirect $ ReferAFriendIR defaultLanguage referrer

getReferAFriendIR :: Language -> Text -> Handler Html
getReferAFriendIR lang referrer = do
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  setLanguage' lang
  messageRender <- getMessageRender
  mmsg <- getMessage
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm lang messageRender (Just referrer)
  let route lang' = ReferAFriendIR lang' referrer
  internationalLayout lang $ do
    setTitleI MsgReferralSignupTitle
    $(widgetFile "homepage")
