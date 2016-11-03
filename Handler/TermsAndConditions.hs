module Handler.TermsAndConditions where

import Import

import Handler.Widgets (languageSelectorWidget, signupBannerWidget,
                        termsAndConditionsTextWidget)

-- | Show the terms and conditions on a stand-alone page.
getTermsAndConditionsR :: Handler Html
getTermsAndConditionsR = redirect $ TermsAndConditionsIR defaultLanguage

getTermsAndConditionsIR :: Language -> Handler Html
getTermsAndConditionsIR lang = do
  internationalLayout lang $ do
    setTitleI MsgTermsAndConditionsTitle
    $(widgetFile "terms-and-conditions")
  where
    route = TermsAndConditionsIR
