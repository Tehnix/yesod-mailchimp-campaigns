{-# LANGUAGE FlexibleInstances #-}
module Types where

import ClassyPrelude.Yesod


data JobAction = SendActiviatonMail
               | SendWelcomeMail
               deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "JobAction"
