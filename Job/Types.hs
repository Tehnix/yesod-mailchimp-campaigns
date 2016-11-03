{-# LANGUAGE FlexibleInstances #-}
module Job.Types where

import ClassyPrelude.Yesod

type Mail = Text

-- | The different actions a job can be.
data JobAction = SendActiviatonMail
               | SendWelcomeMail
               | SendStepAchievedMail
               deriving (Show, Read, Eq, Ord, Bounded)
derivePersistField "JobAction"

-- | The values tha can be passed to a job.
data JobValue = JobValueUserID Int
              | JobValueUserMail Mail
              | JobValueStepNumber Mail Int
              deriving  (Show, Read, Eq)
derivePersistField "JobValue"
