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
      let dashboardBannerImage = case lang of
            Danish    -> render $ StaticR images_dashboardbannerda_jpg
            Swedish   -> render $ StaticR images_dashboardbannerse_jpg
            Norwegian -> render $ StaticR images_dashboardbannerno_jpg
      referralCount <- runDB $ count [ UserReferredBy ==. Just userId
                                     , UserActivated ==. True
                                     ]
      let progressBarPercentage = 20 + (referralCount * 4)
      let progressBarMobilePercentage = referralCount * 4
      defaultLayout $ do
        setTitleI MsgDashboardTitle
        $(widgetFile "dashboard")
  where
    flippedRoute lang' = DashboardIR lang' dashboardToken
