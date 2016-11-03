module Handler.Widgets where

import           Import

import qualified Data.Text as T

-- | Pull in the signup banner widget.
signupBannerWidget :: Widget
signupBannerWidget = do
  messageRender <- getMessageRender
  let msgRender a = rawJS $ messageRender a
  $(widgetFile "signup-banner")

-- | Pull in the terms and conditions text as a widget.
termsAndConditionsTextWidget :: Widget
termsAndConditionsTextWidget = do
  $(widgetFile "terms-and-conditions-text")

-- | Language selector widget.
languageSelectorWidget :: Language -> (Language -> Route App) -> Widget
languageSelectorWidget lang route = do
  messageRender <- getMessageRender
  $(widgetFile "language-selector")
