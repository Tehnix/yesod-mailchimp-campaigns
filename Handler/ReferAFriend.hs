module Handler.ReferAFriend where

import Import

import Handler.SignupForm (signupForm)
import Handler.Widgets    (languageSelectorWidget, signupBannerWidget)

-- | The signup page with a referrer.
getReferAFriendR :: Text -> Handler Html
getReferAFriendR referrer = redirect $ ReferAFriendIR defaultLanguage referrer

getReferAFriendIR :: Language -> Text -> Handler Html
getReferAFriendIR lang referrer = do
  messageRender <- getMessageRender
  mmsg <- getMessage
  master <- getYesod
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm
                                             master lang messageRender (Just referrer)
  setUltDestCurrent
  internationalLayout lang $ do
    setTitleI MsgReferralSignupTitle
    $(widgetFile "homepage")
  where
    route lang' = ReferAFriendIR lang' referrer
