module Handler.Error where
import Import


signupBannerWidget :: Widget
signupBannerWidget = $(widgetFile "signup-banner")

getMailAlreadyExistsR :: Handler Html
getMailAlreadyExistsR = do
  let errorMessage = "The mail already exists!" :: Text
  defaultLayout $ do
    setTitle "Mail Already Exists"
    $(widgetFile "error")

getMailInvalidR :: Handler Html
getMailInvalidR = do
  let errorMessage = "Invalid mail!" :: Text
  defaultLayout $ do
    setTitle "Invalid Mail"
    $(widgetFile "error")

getInvalidFormR :: Handler Html
getInvalidFormR = do
  let errorMessage = "There was an error in the form!" :: Text
  defaultLayout $ do
    setTitle "Invalid Form"
    $(widgetFile "error")

getInvalidActivationKeyR :: Handler Html
getInvalidActivationKeyR = do
  let errorMessage = "Invalid activation key!" :: Text
  defaultLayout $ do
    setTitle "Invalid Activation Key"
    $(widgetFile "error")
