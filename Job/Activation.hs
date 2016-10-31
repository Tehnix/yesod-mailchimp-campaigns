module Job.Activation (
  sendActivationMail
 ) where

import           Import
import qualified Network.HTTP.Simple        as HTTP
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Crypto.Hash                as CH


data MailchimpActivate = MailchimpActivate Text Text deriving Show

instance ToJSON MailchimpActivate where
  toJSON (MailchimpActivate email activationLink) =
    object [ "email_address" .= email
           -- ^ Mailchimp user email address
           , "status"        .= ("subscribed" :: Text)
           -- ^ Mailchimp user status (i.e. subscribed, pending etc)
           , "merge_fields"  .= object
                  [ "MMERGE15" .= activationLink
                  -- ^ Activation link mailchimp merge field
                  , "MMERGE18" .= ("Ja" :: Text)
                  -- ^ Add the user to the "Signed Up" group
                  ]
           ]

hexMD5 :: Text -> String
hexMD5 s = show (CH.hash (T.encodeUtf8 s) :: CH.Digest CH.MD5)

-- | Add the user to mailchimp list
sendActivationMail :: Key Job -> JobValue -> HandlerT App IO ()
sendActivationMail jobId (JobValueUserMail mail) = do
  $logInfo $ "Running sendActivationMail job for " <> mail
  -- Get the mailchimp API settings
  master <- getYesod
  let mailchimpApiUser = T.encodeUtf8 . mcApiUser . appMailchimp $ appSettings master
  let mailchimpApiKey = T.encodeUtf8 . mcApiKey . appMailchimp $ appSettings master
  let mailchimpApiLocation = mcApiLocation . appMailchimp $ appSettings master
  now <- liftIO getCurrentTime
  maybeUser <- runDB . getBy $ UniqueEmail mail
  case maybeUser of
    Nothing              -> return ()
    Just (Entity _ user) -> do
      let mailchimpListId = case userLanguage user of
            Danish    -> mcDanishListId . appMailchimp $ appSettings master
            Swedish   -> mcSwedishListId . appMailchimp $ appSettings master
            Norwegian -> mcNorwegianListId . appMailchimp $ appSettings master
      let mailchimpApiEndpoint = T.unpack $ "http://" <> mailchimpApiLocation <> ".api.mailchimp.com/3.0/lists/" <> mailchimpListId <> "/members/"
      render <- getUrlRender
      let activationUrl = render $ ActivateSignupR (userActivationToken user)
      let subscriber = MailchimpActivate mail activationUrl
      let postUrl = parseRequest_ $ "POST " <> mailchimpApiEndpoint
      let postRequest = HTTP.setRequestBasicAuth mailchimpApiUser mailchimpApiKey
                      . HTTP.setRequestIgnoreStatus
                      $ HTTP.setRequestBodyJSON subscriber postUrl
      postResponse <- liftIO $ HTTP.httpLBS postRequest
      -- Check if the API call was successful or not
      case HTTP.getResponseStatusCode postResponse of
        -- Status code 200 indicates the user was successfully added
        200 -> runDB $ update jobId [JobFinished =. True, JobUpdated =. now, JobResult =. Just "Successful"]

        -- If we get a status code 400, the user already exists and we need to
        -- send a PATCH request instead to update their information
        400 -> do
          let mailHash = hexMD5 mail
          let patchUrl = parseRequest_ $ "PATCH " <> mailchimpApiEndpoint <> mailHash
          let patchRequest = HTTP.setRequestBasicAuth mailchimpApiUser mailchimpApiKey
                           . HTTP.setRequestIgnoreStatus
                           $ HTTP.setRequestBodyJSON subscriber patchUrl
          patchResponse <- liftIO $ HTTP.httpLBS patchRequest
          -- Check if the API call was successful or not
          case HTTP.getResponseStatusCode patchResponse of
            200 -> runDB $ update jobId [JobFinished =. True, JobUpdated =. now, JobResult =. Just "Successful"]
            _   -> runDB $ update jobId [JobUpdated =. now, JobResult =. Just (T.decodeUtf8 . C.toStrict $ HTTP.getResponseBody patchResponse)]

        -- Any other status code and the job is marked as failed
        _   -> runDB $ update jobId [JobUpdated =. now, JobResult =. Just "Failed"]

      return ()
sendActivationMail _ _ = return ()
