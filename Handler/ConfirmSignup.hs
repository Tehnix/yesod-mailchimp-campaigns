module Handler.ConfirmSignup where
import           Data.List       (elemIndex)
import           Data.Maybe      (fromJust)
import           Import

import           Handler.Widgets (languageSelectorWidget, signupBannerWidget)


-- | Notify the user that they have signed up
getConfirmSignupIR :: Language -> Handler Html
getConfirmSignupIR lang = do
  setLanguage' lang
  let route = ConfirmSignupIR
  defaultLayout $ do
    setTitleI MsgConfirmSignupTitle
    $(widgetFile "confirm-signup")

-- | Notify the user that they used an invalid activation key
getInvalidActivationKeyIR :: Language -> Handler Html
getInvalidActivationKeyIR lang = do
  setLanguage' lang
  messageRender <- getMessageRender
  let errorMessage = messageRender MsgInvalidActivationKeyTitle
  let route = InvalidActivationKeyIR
  defaultLayout $ do
    setTitleI MsgInvalidActivationKeyTitle
    $(widgetFile "error")

-- | Try to activate the user belonging to the key and redirect to dashboard
getActivateSignupR :: Text -> Handler Html
getActivateSignupR activationToken = redirect $ ActivateSignupIR defaultLanguage activationToken

getActivateSignupIR :: Language -> Text -> Handler Html
getActivateSignupIR lang activationToken = do
  maybeActivation <- runDB . getBy $ UniqueActivationToken activationToken
  case maybeActivation of
    Nothing -> redirect $ InvalidActivationKeyIR lang
    Just (Entity userId user) -> do
      now <- liftIO getCurrentTime
      -- Mark the user as activated and schedule the welcome email to be sent
      _ <- runDB $ do
        update userId [UserActivated =. True]
        insert $ Job SendWelcomeMail (JobValueUserMail (userEmail user)) Nothing 0 False now now
      -- Once the user is activated, check if the referrer has achived the next
      -- step - if so, schedule the achieved step email to be sent
      case userReferredBy user of
        Nothing -> return ()
        Just referrerUserId -> do
          master <- getYesod
          let campaignSteps = cmpSteps . appCampaign $ appSettings master
          referredUsersCount <- runDB $ count [ UserReferredBy ==. userReferredBy user
                                              , UserActivated ==. True
                                              ]
          when (referredUsersCount `elem` campaignSteps) $ do
              let stepNumber = 1 + (fromJust $ referredUsersCount `elemIndex` campaignSteps)
              maybeReferrer <- runDB $ get referrerUserId
              case maybeReferrer of
                Nothing -> return ()
                Just referrerUser -> do
                  _ <- runDB . insert $ Job SendStepAchievedMail (JobValueStepNumber (userEmail referrerUser) stepNumber) Nothing 0 False now now
                  return ()
      -- Finally, redirect the user to their dashboard after activation
      redirect $ DashboardIR lang (userDashboardToken user)
