module Job.HandleJob (
  handleJob
  ) where

import Import
import Job.SendActivationMail
import Job.SendWelcomeMail


-- | Dispatch jobs to their repsective functions
handleJob :: Entity Job -> HandlerT App IO ()
handleJob (Entity jobId job) = do
  runDB $ update jobId [JobAttempt +=. 1]
  case jobAction job of
    SendActiviatonMail -> sendActivationMail jobId $ jobValue job
    SendWelcomeMail    -> sendWelcomeMail jobId $ jobValue job
