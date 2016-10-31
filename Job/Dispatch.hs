module Job.Dispatch (
  dispatchJob
  ) where
import           Import
import           Job.Activation
import           Job.StepAchieved
import           Job.Welcome


-- | Dispatch jobs to their repsective functions
dispatchJob :: Entity Job -> HandlerT App IO ()
dispatchJob (Entity jobId job) = do
  runDB $ update jobId [JobAttempt +=. 1]
  case jobAction job of
    SendActiviatonMail   -> sendActivationMail jobId $ jobValue job
    SendWelcomeMail      -> sendWelcomeMail jobId $ jobValue job
    SendStepAchievedMail -> sendStepAchievedMail jobId $ jobValue job
