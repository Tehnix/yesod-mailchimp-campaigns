module Handler.Klipfolio where
import           Import


data KlipFolioSubscriberCount = KlipFolioSubscriberCount
  { klipfolioSubscriberCountEnabled      :: Bool
  , klipfolioSubscriberCount             :: Int
  , klipfolioSubscriberCountNotActivated :: Int
  }

instance ToJSON KlipFolioSubscriberCount where
  toJSON KlipFolioSubscriberCount {..} = object
    [ "enabled"         .= klipfolioSubscriberCountEnabled
    , "subscriberCount" .= klipfolioSubscriberCount
    , "notActivated"    .= klipfolioSubscriberCountNotActivated
    ]

-- | The signup page without any referrer
getKlipSubscriberCountR :: Text -> Handler Value
getKlipSubscriberCountR endpoint = do
  master <- getYesod
  let maybeKlipfolio = appKlipfolio $ appSettings master
  case maybeKlipfolio of
    Nothing        -> returnJson $ KlipFolioSubscriberCount False 0 0
    Just klipfolio -> do
      let validEndpoint = klipEndpoint klipfolio
      case endpoint == validEndpoint of
        False -> returnJson $ KlipFolioSubscriberCount False 0 0
        True  -> do
          subCount <- runDB $ count [ SignupActivated ==. True ]
          notActivatedCount <- runDB $ count [ SignupActivated ==. False ]
          returnJson $ KlipFolioSubscriberCount True subCount notActivatedCount
