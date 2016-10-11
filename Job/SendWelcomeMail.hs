module Job.SendWelcomeMail (
  sendWelcomeMail
 ) where

import           Import
import qualified Network.HTTP.Simple   as HTTP
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Crypto.Hash           as CH


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
sendWelcomeMail :: Key Job -> Text -> HandlerT App IO ()
sendWelcomeMail jobId mail = do
  $logInfo $ "Running SendActivationMail job for " <> mail
  -- Get the mailchimp API settings
  master <- getYesod
  let mailchimpApiUser = T.encodeUtf8 . mcApiUser . appMailchimp $ appSettings master
  let mailchimpApiKey = T.encodeUtf8 . mcApiKey . appMailchimp $ appSettings master
  let mailchimpApiLocation = mcApiLocation . appMailchimp $ appSettings master
  let mailchimpListId = mcListId . appMailchimp $ appSettings master
  let mailchimpApiEndpoint = T.unpack $ "http://" <> mailchimpApiLocation <> ".api.mailchimp.com/3.0/lists/" <> mailchimpListId <> "/members/"
  now <- liftIO getCurrentTime
  maybeUser <- runDB . getBy $ UniqueEmail mail
  case maybeUser of
    Nothing                   -> return ()
    Just (Entity _ user) -> do
      render <- getUrlRender
      let referralUrl = render $ ReferAFriendR (userReferralToken user)
      let dashboardUrl = render $ DashboardR (userDashboardToken user)
      let subscriber = MailchimpWelcome mail referralUrl dashboardUrl
      let mailHash = hexMD5 mail
      let patchUrl = parseRequest_ $ "PATCH " <> mailchimpApiEndpoint <> mailHash
      let patchRequest = HTTP.setRequestBasicAuth mailchimpApiUser mailchimpApiKey
                       . HTTP.setRequestIgnoreStatus
                       $ HTTP.setRequestBodyJSON subscriber patchUrl
      patchResponse <- liftIO $ HTTP.httpLBS patchRequest
      -- Check if the API call was successful or not
      case HTTP.getResponseStatusCode patchResponse of
        200 -> runDB $ update jobId [JobFinished =. True, JobUpdated =. now, JobResult =. Just "Successful"]
        _   -> runDB $ update jobId [JobUpdated =. now, JobResult =. Just "Failed"]
      return ()
