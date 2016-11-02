module Job.Welcome
  ( sendWelcomeMail
  ) where

import           Import

import qualified Crypto.Hash                as CH
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Network.HTTP.Simple        as HTTP

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

hexMD5 :: Text -> String
hexMD5 s = show (CH.hash (T.encodeUtf8 s) :: CH.Digest CH.MD5)

-- | Add the user to mailchimp list
sendWelcomeMail :: Key Job -> JobValue -> HandlerT App IO ()
sendWelcomeMail jobId (JobValueUserMail mail) = do
  $logInfo $ "Running sendWelcomeMail job for " <> mail
  -- Get the mailchimp API settings
  master <- getYesod
  let maybeGoogleAnalytics = appAnalytics $ appSettings master
  let mailchimpApiUser = T.encodeUtf8 . mcApiUser . appMailchimp $ appSettings master
  let mailchimpApiKey = T.encodeUtf8 . mcApiKey . appMailchimp $ appSettings master
  let mailchimpApiLocation = mcApiLocation . appMailchimp $ appSettings master
  now <- liftIO getCurrentTime
  maybeUser <- runDB . getBy $ UniqueEmail mail
  case maybeUser of
    Nothing              -> return ()
    Just (Entity _ signup) -> do
      let lang = signupLanguage signup
      let mailchimpListId = case lang of
            Danish    -> mcListIdDanish . mcListId . appMailchimp $ appSettings master
            Swedish   -> mcListIdSwedish . mcListId . appMailchimp $ appSettings master
            Norwegian -> mcListIdNorwegian . mcListId . appMailchimp $ appSettings master
      let mailchimpApiEndpoint = T.unpack $ "http://" <> mailchimpApiLocation <> ".api.mailchimp.com/3.0/lists/" <> mailchimpListId <> "/members/"
      render <- getUrlRender
      let utms = case maybeGoogleAnalytics of
            Nothing -> ""
            Just _  -> "?utm_medium=email&utm_campaign=dashboard"
      let referralUrl = render $ ReferAFriendIR lang (signupReferralToken signup)
      let dashboardUrl = render (DashboardIR lang (signupDashboardToken signup)) <> utms
      let subscriber = MailchimpWelcome mail referralUrl dashboardUrl
      let mailHash = hexMD5 mail
      let patchUrl = parseRequest_ $ "PATCH " <> mailchimpApiEndpoint <> mailHash
      let patchRequest = HTTP.setRequestBasicAuth mailchimpApiUser mailchimpApiKey
                       . HTTP.setRequestIgnoreStatus
                       $ HTTP.setRequestBodyJSON subscriber patchUrl
      patchResponse <- liftIO $ HTTP.httpLBS patchRequest
      let resp = T.decodeUtf8 . C.toStrict $ HTTP.getResponseBody patchResponse
      -- Check if the API call was successful or not
      case HTTP.getResponseStatusCode patchResponse of
        200 -> runDB $ update jobId [JobFinished =. True, JobUpdated =. now, JobResult =. Just resp]
        _   -> runDB $ update jobId [JobUpdated =. now, JobResult =. Just resp]
      return ()
sendWelcomeMail _ _ = return ()
