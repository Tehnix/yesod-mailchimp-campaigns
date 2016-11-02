module Handler.DataApi where
import           Import


data DataSubscribers = DataSubscribers
  { dataSubscribersEnabled      :: Bool
  , dataSubscribersActivated    :: Int
  , dataSubscribersNotActivated :: Int
  , dataSubscribersTotal        :: Int
  }

instance ToJSON DataSubscribers where
  toJSON DataSubscribers {..} = object
    [ "enabled"         .= dataSubscribersEnabled
    , "activated"       .= dataSubscribersActivated
    , "notActivated"    .= dataSubscribersNotActivated
    , "total"           .= dataSubscribersTotal
    ]

-- | Report how many subscribers there are
getDataSubscribersR :: Text -> Handler Value
getDataSubscribersR endpoint = do
  master <- getYesod
  let emptyResponse = returnJson $ DataSubscribers False 0 0 0
  let maybeDataApi = appDataApi $ appSettings master
  case maybeDataApi of
    Nothing        -> emptyResponse
    Just dataApi -> do
      let validEndpoint = dataApiEndpoint dataApi
      case endpoint == validEndpoint of
        False -> emptyResponse
        True  -> do
          total <- runDB $ count ([] :: [Filter Signup])
          activated <- runDB $ count [SignupActivated ==. True]
          notActivated <- runDB $ count [SignupActivated ==. False]
          returnJson $ DataSubscribers True activated notActivated total

data DataReachedSteps = DataReachedSteps
  { dataReachedStepEnabled     :: Bool
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
    [ "enabled"     .= dataReachedStepEnabled
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
-- jobs have been queued
getDataStepsR :: Text -> Handler Value
getDataStepsR endpoint = do
  master <- getYesod
  let emptyResponseSteps = DataSteps 0 0 0 0
  let emptyResponse = returnJson $ DataReachedSteps False 0 emptyResponseSteps emptyResponseSteps
  let maybeDataApi = appDataApi $ appSettings master
  case maybeDataApi of
    Nothing        -> emptyResponse
    Just dataApi -> do
      let validEndpoint = dataApiEndpoint dataApi
      case endpoint == validEndpoint of
        False -> emptyResponse
        True  -> do
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
          returnJson $ DataReachedSteps True noSteps accumulatedSteps uniqueSteps
  where
    matchesStep :: Int -> Entity Job -> Bool
    matchesStep step (Entity _ job) = let (JobValueStepNumber _ s) = jobValue job in step == s

data DataFailedJob = DataFailedJob
  { dataFailedJobEnabled          :: Bool
  , dataFailedJobTotal            :: Int
  , dataFailedJobMemberExists     :: [Text]
  , dataFailedJobResourceNotFound :: [Text]
  }

instance ToJSON DataFailedJob where
  toJSON DataFailedJob {..} = object
    [ "enabled"          .= dataFailedJobEnabled
    , "total"            .= dataFailedJobTotal
    , "memberExists"     .= dataFailedJobMemberExists
    , "resourceNotFound" .= dataFailedJobResourceNotFound
    ]

-- | Report how jobs failed, and show the emails classified under certain errors
getDataFailedJobR :: Text -> Handler Value
getDataFailedJobR endpoint = do
  master <- getYesod
  let emptyResponse = returnJson $ DataFailedJob False 0 [] []
  let maybeDataApi = appDataApi $ appSettings master
  case maybeDataApi of
    Nothing        -> emptyResponse
    Just dataApi -> do
      let validEndpoint = dataApiEndpoint dataApi
      case endpoint == validEndpoint of
        False -> emptyResponse
        True  -> do
          failedJobs <- runDB $ selectList [JobFinished ==. False, JobAttempt >. 0] []
          let memberExists = extractMail <$> filter (containsText "Member Exists") failedJobs
          let resourceNotFound = extractMail <$> filter (containsText "Resource Not Found") failedJobs
          returnJson $ DataFailedJob True (length failedJobs) memberExists resourceNotFound
  where
    containsText :: Text -> Entity Job -> Bool
    containsText match (Entity _ job) = match `isInfixOf` (fromMaybe "" (jobResult job))
    extractMail :: Entity Job -> Text
    extractMail (Entity _ job) = case jobValue job of
      JobValueUserMail mail     -> mail
      JobValueStepNumber mail _ -> mail
      JobValueUserID _          -> ""
