module Job.Welcome
  ( sendWelcomeMail
  ) where

import           Import

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Network.HTTP.Simple        as HTTP

import           Job.Common

data MailchimpWelcome = MailchimpWelcome Text Text Text deriving Show

instance ToJSON MailchimpWelcome where
  toJSON (MailchimpWelcome email referralLink dashboardLink) =
    object [ "email_address" .= email
           -- ^ Mailchimp user email address
           , "status"        .= ("subscribed" :: Text)
           -- ^ Mailchimp user status (i.e. subscribed, pending etc)
           , "merge_fields"  .= object
                  [ "MMERGE16" .= referralLink
                  -- ^ Referral link mailchimp merge field
                  , "MMERGE17" .= dashboardLink
                  -- ^ Dashboard link mailchimp merge field
                  , "MMERGE19" .= ("Ja" :: Text)
                  -- ^ Add the user to the "Confirmed" group
                  ]
           ]

-- | Trigger the Mailchimp welcome mail.
sendWelcomeMail :: Key Job -> JobValue -> HandlerT App IO ()
sendWelcomeMail jobId (JobValueUserMail mail') = do
  let mail = T.toLower mail'
  $logInfo $ "Running sendWelcomeMail job for " <> mail
  master <- getYesod
  maybeUser <- runDB . getBy $ UniqueEmail mail
  case maybeUser of
    Nothing              -> return ()
    Just (Entity _ signup) -> do
      render <- getUrlRender
      now <- liftIO getCurrentTime
      let lang = signupLanguage signup
      let utms = case appAnalytics $ appSettings master of
            Nothing -> ""
            Just _  -> "?utm_medium=email&utm_campaign=dashboard"
      let referralUrl = render $ ReferAFriendIR lang (signupReferralToken signup)
      let dashboardUrl = render (DashboardIR lang (signupDashboardToken signup)) <> utms
      let subscriber = MailchimpWelcome mail referralUrl dashboardUrl
      let patchRequest = mailchimpPatchRequest master lang subscriber mail
      patchResponse <- liftIO $ HTTP.httpLBS patchRequest
      let resp = T.decodeUtf8 . C.toStrict $ HTTP.getResponseBody patchResponse
      -- Check if the API call was successful or not.
      case HTTP.getResponseStatusCode patchResponse of
        200 -> runDB $ update jobId [JobFinished =. True, JobUpdated =. now, JobResult =. Just resp]
        _   -> runDB $ update jobId [JobUpdated =. now, JobResult =. Just resp]
      return ()
sendWelcomeMail _ _ = return ()
