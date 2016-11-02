{-# LANGUAGE FlexibleInstances #-}
module Job.Types where

import ClassyPrelude.Yesod

data JobAction = SendActiviatonMail
               | SendWelcomeMail
               | SendStepAchievedMail
               deriving (Show, Read, Eq, Ord, Bounded)
derivePersistField "JobAction"

type Mail = Text

data JobValue = JobValueUserID Int
              | JobValueUserMail Mail
              | JobValueStepNumber Mail Int
              deriving  (Show, Read, Eq)
derivePersistField "JobValue"
