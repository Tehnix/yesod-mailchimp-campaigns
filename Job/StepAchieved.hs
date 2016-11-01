module Job.StepAchieved (
  sendStepAchievedMail
 ) where

import           Import
import qualified Network.HTTP.Simple        as HTTP
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Crypto.Hash                as CH


data MailchimpStepAchieved = MailchimpStepAchieved Text Int deriving Show

instance ToJSON MailchimpStepAchieved where
  toJSON (MailchimpStepAchieved email stepNumber) =
    object [ "email_address" .= email
           -- ^ Mailchimp user email address
           , "merge_fields"  .= object
                  [ "MMERGE20" .= stepNumber
                  -- ^ Step number mailchimp merge field
                  ]
           ]

hexMD5 :: Text -> String
hexMD5 s = show (CH.hash (T.encodeUtf8 s) :: CH.Digest CH.MD5)

-- | Update the step number that the user has achieved
sendStepAchievedMail :: Key Job -> JobValue -> HandlerT App IO ()
sendStepAchievedMail jobId (JobValueStepNumber mail stepNumber) = do
  $logInfo $ "Running sendStepAchievedMail job for " <> mail
  -- Get the mailchimp API settings
  master <- getYesod
  let mailchimpApiUser = T.encodeUtf8 . mcApiUser . appMailchimp $ appSettings master
  let mailchimpApiKey = T.encodeUtf8 . mcApiKey . appMailchimp $ appSettings master
  let mailchimpApiLocation = mcApiLocation . appMailchimp $ appSettings master
  now <- liftIO getCurrentTime
  maybeUser <- runDB . getBy $ UniqueEmail mail
  case maybeUser of
    Nothing           -> return ()
    Just (Entity _ signup) -> do
      let mailchimpListId = case signupLanguage signup of
            Danish    -> mcListIdDanish . mcListId . appMailchimp $ appSettings master
            Swedish   -> mcListIdSwedish . mcListId . appMailchimp $ appSettings master
            Norwegian -> mcListIdNorwegian . mcListId . appMailchimp $ appSettings master
      let mailchimpApiEndpoint = T.unpack $ "http://" <> mailchimpApiLocation <> ".api.mailchimp.com/3.0/lists/" <> mailchimpListId <> "/members/"
      let subscriber = MailchimpStepAchieved mail stepNumber
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
sendStepAchievedMail _ _ = return ()
