module Handler.DataApi where

import Import

data DataErrorResponse = DataErrorResponse
  { dataErrorResponseStatus  :: Int
  , dataErrorResponseMessage :: Text
  }

instance ToJSON DataErrorResponse where
  toJSON DataErrorResponse {..} = object
    [ "status"  .= dataErrorResponseStatus
    , "message" .= dataErrorResponseMessage
    ]

-- | Check if the endpoint has the valid key and return a response if it has,
-- else return an error response.
maybeEndpointResponse :: Text -> Handler Value -> Handler Value
maybeEndpointResponse endpoint response = do
  master <- getYesod
  let accessForbidden = returnJson $ DataErrorResponse 403 "Access forbidden"
  let missingSettings = returnJson $ DataErrorResponse 501 "Endpoint not enabled"
  case appDataApi $ appSettings master of
    Nothing      -> missingSettings
    Just dataApi -> if endpoint == dataApiEndpoint dataApi
                      then response
                      else accessForbidden

data DataSubscribers = DataSubscribers
  { dataSubscribersStatus       :: Int
  , dataSubscribersActivated    :: Int
  , dataSubscribersNotActivated :: Int
  , dataSubscribersTotal        :: Int
  }

instance ToJSON DataSubscribers where
  toJSON DataSubscribers {..} = object
    [ "status"          .= dataSubscribersStatus
    , "activated"       .= dataSubscribersActivated
    , "notActivated"    .= dataSubscribersNotActivated
    , "total"           .= dataSubscribersTotal
    ]

-- | Report how many subscribers there are.
getDataSubscribersR :: Text -> Handler Value
getDataSubscribersR endpoint = maybeEndpointResponse endpoint $ do
  total <- runDB $ count ([] :: [Filter Signup])
  activated <- runDB $ count [SignupActivated ==. True]
  notActivated <- runDB $ count [SignupActivated ==. False]
  returnJson $ DataSubscribers 200 activated notActivated total

data DataReachedSteps = DataReachedSteps
  { dataReachedStepStatus      :: Int
  , dataReachedStepNoSteps     :: Int
  , dataReachedStepAccumulated :: DataSteps
  , dataReachedStepUnique      :: DataSteps
  }

data DataSteps = DataSteps
  { dataReachedStep1 :: Int
  , dataReachedStep2 :: Int
  , dataReachedStep3 :: Int
  , dataReachedStep4 :: Int
  }

instance ToJSON DataReachedSteps where
  toJSON DataReachedSteps {..} = object
    [ "status"      .= dataReachedStepStatus
    , "noSteps"     .= dataReachedStepNoSteps
    , "accumulated" .= dataReachedStepAccumulated
    , "unique"      .= dataReachedStepUnique
    ]

instance ToJSON DataSteps where
  toJSON DataSteps {..} = object
    [ "step1" .= dataReachedStep1
    , "step2" .= dataReachedStep2
    , "step3" .= dataReachedStep3
    , "step4" .= dataReachedStep4
    ]

-- | Report how many have reached each step, by looking at how many SendStepAchievedMail
-- jobs have been queued.
getDataStepsR :: Text -> Handler Value
getDataStepsR endpoint = maybeEndpointResponse endpoint $ do
  activatedSignups <- runDB $ count [SignupActivated ==. True]
  stepsAchieved <- runDB $ selectList [JobAction ==. SendStepAchievedMail] []
  let step4 = length $ filter (matchesStep 4) stepsAchieved
  let step3 = length $ filter (matchesStep 3) stepsAchieved
  let step2 = length $ filter (matchesStep 2) stepsAchieved
  let step1 = length $ filter (matchesStep 1) stepsAchieved
  let noSteps = activatedSignups - step1
  let accumulatedSteps = DataSteps step1 step2 step3 step4
  let uniqueSteps = DataSteps
                           (step1 - step2)
                           (step2 - step3)
                           (step3 - step4)
                           step4
  returnJson $ DataReachedSteps 200 noSteps accumulatedSteps uniqueSteps
  where
    matchesStep :: Int -> Entity Job -> Bool
    matchesStep step (Entity _ job) = let (JobValueStepNumber _ s) = jobValue job in step == s

data DataFailedJob = DataFailedJob
  { dataFailedJobStatus           :: Int
  , dataFailedJobTotal            :: Int
  , dataFailedJobMemberExists     :: [Text]
  , dataFailedJobResourceNotFound :: [Text]
  }

instance ToJSON DataFailedJob where
  toJSON DataFailedJob {..} = object
    [ "status"           .= dataFailedJobStatus
    , "total"            .= dataFailedJobTotal
    , "memberExists"     .= dataFailedJobMemberExists
    , "resourceNotFound" .= dataFailedJobResourceNotFound
    ]

-- | Report how jobs failed, and show the emails classified under certain errors.
getDataFailedJobR :: Text -> Handler Value
getDataFailedJobR endpoint = maybeEndpointResponse endpoint $ do
  failedJobs <- runDB $ selectList [JobFinished ==. False, JobAttempt >. 0] []
  let memberExists = extractMail <$> filter (containsText "Member Exists") failedJobs
  let resourceNotFound = extractMail <$> filter (containsText "Resource Not Found") failedJobs
  returnJson $ DataFailedJob 200 (length failedJobs) memberExists resourceNotFound
  where
    containsText :: Text -> Entity Job -> Bool
    containsText match (Entity _ job) = match `isInfixOf` (fromMaybe "" (jobResult job))

    extractMail :: Entity Job -> Text
    extractMail (Entity _ job) = case jobValue job of
      JobValueUserMail mail     -> mail
      JobValueStepNumber mail _ -> mail
      JobValueUserID _          -> ""
