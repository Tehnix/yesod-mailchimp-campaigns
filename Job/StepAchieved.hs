module Job.StepAchieved
  ( sendStepAchievedMail
  ) where

import           Import

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Encoding         as T
import qualified Network.HTTP.Simple        as HTTP

import           Job.Common

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

-- | Update the step number that the user has achieved.
sendStepAchievedMail :: Key Job -> JobValue -> HandlerT App IO ()
sendStepAchievedMail jobId (JobValueStepNumber mail stepNumber) = do
  $logInfo $ "Running sendStepAchievedMail job for " <> mail
  master <- getYesod
  maybeUser <- runDB . getBy $ UniqueEmail mail
  case maybeUser of
    Nothing                -> return ()
    Just (Entity _ signup) -> do
      now <- liftIO getCurrentTime
      let lang = signupLanguage signup
      let subscriber = MailchimpStepAchieved mail stepNumber
      let patchRequest = mailchimpPatchRequest master lang subscriber mail
      patchResponse <- liftIO $ HTTP.httpLBS patchRequest
      let resp = T.decodeUtf8 . C.toStrict $ HTTP.getResponseBody patchResponse
      -- Check if the API call was successful or not.
      case HTTP.getResponseStatusCode patchResponse of
        200 -> runDB $ update jobId [JobFinished =. True, JobUpdated =. now, JobResult =. Just resp]
        _   -> runDB $ update jobId [JobUpdated =. now, JobResult =. Just resp]
      return ()
sendStepAchievedMail _ _ = return ()
