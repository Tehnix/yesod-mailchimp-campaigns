module Job.Tracking (
  sendGoogleAnalyticsTracking
 ) where

import           Import
import qualified Network.HTTP.Simple        as HTTP
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.UUID                  as UU
import qualified Data.UUID.V1               as UU


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

sendTrackingEvent :: MonadIO m => Text -> m (Either Text Text)
sendTrackingEvent payload = do
  let postUrl = parseRequest_ $ "POST " <> googleAnalyticsApiEndpoint
  let requestPayload = C.fromStrict $ T.encodeUtf8 payload
  let postRequest = HTTP.setRequestIgnoreStatus
                  $ HTTP.setRequestBodyLBS requestPayload postUrl
  postResponse <- liftIO $ HTTP.httpLBS postRequest
  let postResp = T.decodeUtf8 . C.toStrict $ HTTP.getResponseBody postResponse
  case HTTP.getResponseStatusCode postResponse of
    200 -> return $ Right postResp
    _   -> return $ Left postResp

-- | Track the event with Google Analytics
sendGoogleAnalyticsTracking :: Text -> JobAction -> JobValue -> HandlerT App IO ()
sendGoogleAnalyticsTracking uaCode SendActiviatonMail (JobValueUserMail mail) = do
  maybeSignup <- runDB . getBy $ UniqueEmail mail
  case maybeSignup of
    Just (Entity signupId _) -> do
      let uid = T.pack $ show signupId
      payload <- liftIO $ mkTrackingPayload uaCode "activation+mail" uid Nothing
      _ <- sendTrackingEvent payload
      return ()
    Nothing -> return ()
sendGoogleAnalyticsTracking uaCode SendWelcomeMail (JobValueUserMail mail) = do
  maybeSignup <- runDB . getBy $ UniqueEmail mail
  case maybeSignup of
    Just (Entity signupId _) -> do
      let uid = T.pack $ show signupId
      payload <- liftIO $ mkTrackingPayload uaCode "welcome+mail" uid Nothing
      _ <- sendTrackingEvent payload
      return ()
    Nothing -> return ()
sendGoogleAnalyticsTracking uaCode SendStepAchievedMail (JobValueStepNumber mail stepNumber) = do
  maybeSignup <- runDB . getBy $ UniqueEmail mail
  case maybeSignup of
    Just (Entity signupId _) -> do
      let uid = T.pack $ show signupId
      let step = T.pack $ show stepNumber
      payload <- liftIO $ mkTrackingPayload uaCode "welcome+mail" uid (Just step)
      _ <- sendTrackingEvent payload
      return ()
    Nothing -> return ()
sendGoogleAnalyticsTracking _ _ _ = return ()