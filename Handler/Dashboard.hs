module Handler.Dashboard where
import Import


getDashboardR :: Text -> Handler Html
getDashboardR dashboardToken = do
  maybeDashboard <- runDB . getBy $ UniqueDashboardToken dashboardToken
  case maybeDashboard of
    Nothing -> do
      setMessage "Not a valid dashboard key!"
      redirect SignupR
    Just (Entity userId user) -> do
      let referralToken = userReferralToken user
      referredUsers <- runDB $ selectList [UserReferredBy ==. Just userId, UserActivated ==. True] []
      let referralCount = length referredUsers
      let progressBarPercentage = 20 + (referralCount * 4)
      defaultLayout $ do
        setTitle "Dashboard"
        $(widgetFile "dashboard")
