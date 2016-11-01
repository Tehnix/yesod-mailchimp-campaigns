module Handler.Dashboard where
import qualified Data.Text         as T
import           Import
import qualified Network.HTTP.Base as HTTP (urlEncode)

import           Handler.Widgets   (languageSelectorWidget)


-- | A dashboard showing the progress of the users referrals
getDashboardR :: Text -> Handler Html
getDashboardR dashboardToken = redirect $ DashboardIR defaultLanguage dashboardToken

getDashboardIR :: Language -> Text -> Handler Html
getDashboardIR lang dashboardToken = do
  setLanguage' lang
  render <- getUrlRender
  messageRender <- getMessageRender
  maybeDashboard <- runDB . getBy $ UniqueDashboardToken dashboardToken
  let route = flippedRoute
  case maybeDashboard of
    Nothing -> do
      setMessageI MsgNotAValidDashboardKey
      redirect $ SignupIR lang
    Just (Entity userId user) -> do
      let referralToken = userReferralToken user
      let referralUrl = render $ ReferAFriendIR lang referralToken
      let encodedReferralUrl = HTTP.urlEncode $ T.unpack referralUrl
      let encodedFacebookShareTitle = HTTP.urlEncode . T.unpack $ messageRender MsgFacebookShareTitle
      let encodedFacebookShareBody = HTTP.urlEncode . T.unpack $ messageRender MsgFacebookShareBody
      let dashboardBannerImage = case lang of
            Danish    -> render $ StaticR images_dashboardBannerDa_jpg
            Swedish   -> render $ StaticR images_dashboardBannerSe_jpg
            Norwegian -> render $ StaticR images_dashboardBannerNo_jpg
      referralCount <- runDB $ count [ UserReferredBy ==. Just userId
                                     , UserActivated ==. True
                                     ]
      let progressBarPercentage = 20 + (referralCount * 4)
      let progressBarMobilePercentage = referralCount * 4
      internationalLayout lang $ do
        setTitleI MsgDashboardTitle
        $(widgetFile "dashboard")
  where
    flippedRoute lang' = DashboardIR lang' dashboardToken
