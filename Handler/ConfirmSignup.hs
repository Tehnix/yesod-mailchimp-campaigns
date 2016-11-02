module Handler.ConfirmSignup where

import Import

import Data.List       (elemIndex)
import Data.Maybe      (fromJust)

import Handler.Widgets (languageSelectorWidget, signupBannerWidget)

-- | Notify the user that they have signed up
getConfirmSignupIR :: Language -> Handler Html
getConfirmSignupIR lang = do
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  setLanguage' lang
  let route = ConfirmSignupIR
  internationalLayout lang $ do
    setTitleI MsgConfirmSignupTitle
    $(widgetFile "confirm-signup")

-- | Notify the user that they used an invalid activation key
getInvalidActivationKeyIR :: Language -> Handler Html
getInvalidActivationKeyIR lang = do
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  setLanguage' lang
  messageRender <- getMessageRender
  let errorMessage = messageRender MsgInvalidActivationKeyTitle
  let route = InvalidActivationKeyIR
  internationalLayout lang $ do
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
    Just (Entity signupId signup) -> do
      now <- liftIO getCurrentTime
      -- Mark the signup as activated and schedule the welcome email to be sent
      _ <- runDB $ do
        update signupId [SignupActivated =. True]
        insert $ Job SendWelcomeMail (JobValueUserMail (signupEmail signup)) Nothing 0 False now now
      -- Once the signup is activated, check if the referrer has achived the next
      -- step - if so, schedule the achieved step email to be sent
      case signupReferredBy signup of
        Nothing -> return ()
        Just referrerUserId -> do
          master <- getYesod
          let campaignSteps = cmpSteps . appCampaign $ appSettings master
          referredUsersCount <- runDB $ count [ SignupReferredBy ==. signupReferredBy signup
                                              , SignupActivated ==. True
                                              ]
          when (referredUsersCount `elem` campaignSteps) $ do
              let stepNumber = 1 + (fromJust $ referredUsersCount `elemIndex` campaignSteps)
              maybeReferrer <- runDB $ get referrerUserId
              case maybeReferrer of
                Nothing -> return ()
                Just referrerUser -> do
                  _ <- runDB . insert $ Job SendStepAchievedMail (JobValueStepNumber (signupEmail referrerUser) stepNumber) Nothing 0 False now now
                  return ()
      -- Finally, redirect the signup to their dashboard after activation
      redirect $ DashboardIR lang (signupDashboardToken signup)
