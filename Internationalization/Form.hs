module Internationalization.Form where
import           Import.NoFoundation


danishFormMessage :: FormMessage -> Text
danishFormMessage MsgValueRequired = "Indtast venligst en email!"
danishFormMessage a                = defaultFormMessage a

swedishFormMessage :: FormMessage -> Text
swedishFormMessage MsgValueRequired = "Indtast venligst en email!"
swedishFormMessage a                = defaultFormMessage a

norwegianFormMessage :: FormMessage -> Text
norwegianFormMessage MsgValueRequired = "Indtast venligst en email!"
norwegianFormMessage a                = defaultFormMessage a
