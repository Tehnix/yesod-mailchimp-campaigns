module Handler.Job where
import           Import
import           Job.Dispatch


getRunJobsR :: Handler Html
getRunJobsR = do
  jobs <- runDB $ selectList [JobFinished ==. False, JobAttempt <. 100] []
  mapM_ dispatchJob jobs
  defaultLayout $ do
    setTitle "Job Runner"
    $(widgetFile "force-run-jobs")
