module Handler.Job where
import Import
import Job.HandleJob


getRunJobsR :: Handler Html
getRunJobsR = do
  jobs <- runDB $ selectList [JobFinished ==. False] []
  mapM_ handleJob jobs
  defaultLayout $ do
    setTitle "Job Runner"
    $(widgetFile "force-run-jobs")
