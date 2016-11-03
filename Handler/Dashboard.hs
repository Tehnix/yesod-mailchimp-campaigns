module Handler.Dashboard where

import           Import

import qualified Data.Text         as T
import qualified Network.HTTP.Base as HTTP (urlEncode)

import           Handler.Widgets   (languageSelectorWidget)

-- | A dashboard showing the progress of the users referrals.
getDashboardR :: Text -> Handler Html
getDashboardR dashboardToken = redirect $ DashboardIR defaultLanguage dashboardToken

getDashboardIR :: Language -> Text -> Handler Html
getDashboardIR lang dashboardToken = do
  render <- getUrlRender
  messageRender <- getMessageRender
  master <- getYesod
  maybeDashboard <- runDB . getBy $ UniqueDashboardToken dashboardToken
  case maybeDashboard of
    Nothing -> do
      setMessageI MsgNotAValidDashboardKey
      redirect $ SignupIR lang
    Just (Entity signupId signup) -> do
      let utmsFacebook = case appAnalytics $ appSettings master of
            Nothing -> ""
            Just _  -> "?utm_medium=facebook&utm_campaign=referral"
      let utmsDirectLink = case appAnalytics $ appSettings master of
            Nothing -> rawJS ("" :: Text)
            Just _  -> rawJS ("?utm_medium=direct+link&utm_campaign=referral" :: Text)
      let referralToken = signupReferralToken signup
      let referralUrl = render $ ReferAFriendIR lang referralToken
      let encodedReferralUrl = HTTP.urlEncode $ T.unpack referralUrl <> utmsFacebook
      let encodedFacebookShareTitle = HTTP.urlEncode . T.unpack $ messageRender MsgFacebookShareTitle
      let encodedFacebookShareBody = HTTP.urlEncode . T.unpack $ messageRender MsgFacebookShareBody
      let dashboardBannerImage = case lang of
            Danish    -> render $ StaticR images_dashboardBannerDa_jpg
            Swedish   -> render $ StaticR images_dashboardBannerSe_jpg
            Norwegian -> render $ StaticR images_dashboardBannerNo_jpg
      referralCount <- runDB $ count [ SignupReferredBy ==. Just signupId
                                     , SignupActivated ==. True
                                     ]
      let progressBarPercentage = 20 + (referralCount * 4)
      let progressBarMobilePercentage = referralCount * 4
      internationalLayout lang $ do
        setTitleI MsgDashboardTitle
        $(widgetFile "dashboard")
  where
    route lang' = DashboardIR lang' dashboardToken
