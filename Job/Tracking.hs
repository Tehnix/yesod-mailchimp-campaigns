module Job.Tracking
  ( sendGoogleAnalyticsTracking
  ) where

import           Import

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.UUID                  as UU
import qualified Data.UUID.V1               as UU
import qualified Network.HTTP.Simple        as HTTP

googleAnalyticsApiEndpoint :: String
googleAnalyticsApiEndpoint = "http://www.google-analytics.com/collect"

mkTrackingPayload :: Text -> Text -> Text -> Maybe Text -> IO Text
mkTrackingPayload uaCode jobName uid content = do
  let utmContent = case content of
        Nothing -> ""
        Just c  -> "&cc=" <> c
  maybeUUID <- UU.nextUUID
  case maybeUUID of
    Nothing   -> return ""
    Just uuid -> return $ "v=1&t=event&ds=backend&tid=" <> uaCode <> "&cid=" <> (UU.toText uuid) <> "&uid=" <> uid <> "&cn=" <> jobName <> "&cm=backend&ni=1" <> utmContent

trackingPostRequest :: MonadIO m => Text -> m (Either Text Text)
trackingPostRequest payload = do
  let postUrl = parseRequest_ $ "POST " <> googleAnalyticsApiEndpoint
  let requestPayload = C.fromStrict $ T.encodeUtf8 payload
  let postRequest = HTTP.setRequestIgnoreStatus
                  $ HTTP.setRequestBodyLBS requestPayload postUrl
  postResponse <- liftIO $ HTTP.httpLBS postRequest
  let postResp = T.decodeUtf8 . C.toStrict $ HTTP.getResponseBody postResponse
  case HTTP.getResponseStatusCode postResponse of
    200 -> return $ Right postResp
    _   -> return $ Left postResp

sendTrackingEvent :: Text -> Maybe (Entity Signup) -> Text -> Maybe Text -> HandlerT App IO ()
sendTrackingEvent uaCode maybeSignup jobName content = do
  case maybeSignup of
    Just (Entity signupId _) -> do
      let uid = T.pack $ show signupId
      payload <- liftIO $ mkTrackingPayload uaCode jobName uid content
      _ <- trackingPostRequest payload
      return ()
    Nothing -> return ()

-- | Track the events with Google Analytics.
sendGoogleAnalyticsTracking :: Text -> JobAction -> JobValue -> HandlerT App IO ()
sendGoogleAnalyticsTracking uaCode SendActiviatonMail (JobValueUserMail mail) = do
  maybeSignup <- runDB . getBy $ UniqueEmail mail
  sendTrackingEvent uaCode maybeSignup "activation+mail" Nothing
sendGoogleAnalyticsTracking uaCode SendWelcomeMail (JobValueUserMail mail) = do
  maybeSignup <- runDB . getBy $ UniqueEmail mail
  sendTrackingEvent uaCode maybeSignup "welcome+mail" Nothing
sendGoogleAnalyticsTracking uaCode SendStepAchievedMail (JobValueStepNumber mail stepNumber) = do
  maybeSignup <- runDB . getBy $ UniqueEmail mail
  let step = T.pack $ show stepNumber
  sendTrackingEvent uaCode maybeSignup "step+achieved" (Just step)
sendGoogleAnalyticsTracking _ _ _ = return ()
