module Handler.ConfirmSignup where
import           Import


signupBannerWidget :: Widget
signupBannerWidget = $(widgetFile "signup-banner")

getConfirmSignupR :: Handler Html
getConfirmSignupR = do
  defaultLayout $ do
    setTitle "Confirm Your Signup!"
    $(widgetFile "confirm-signup")

getActivateSignupR :: Text -> Handler Html
getActivateSignupR activationToken = do
  maybeActivation <- runDB . getBy $ UniqueActivationToken activationToken
  case maybeActivation of
    Nothing -> redirect InvalidActivationKeyR
    Just (Entity userId user) -> do
      _ <- runDB $ do
        update userId [UserActivated =. True]
        now <- liftIO getCurrentTime
        insert $ Job SendWelcomeMail (userEmail user) Nothing 0 False now now
      redirect $ DashboardR (userDashboardToken user)
