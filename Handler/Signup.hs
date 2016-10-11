module Handler.Signup where
import           Import
import qualified Data.Text as T
import           System.Random
import qualified Network.HTTP.Base as HTTP (urlEncode)
import           Data.Char (ord, chr)
-- import           Settings.StaticFiles


data Signup = Signup
  { signupEmail    :: Text
  , signupReferrer :: Maybe Text
  } deriving Show

-- | Construct the mail signup form
signupForm :: Maybe Text -> Html -> MForm Handler (FormResult Signup, Widget)
signupForm referrer = renderDivs $ Signup
 <$> areq signupEmailField (FieldSettings "" Nothing (Just "email-input") Nothing [("placeholder", "Indtast din email...")]) Nothing
 <*> case referrer of
   Nothing  -> aopt hiddenField "" Nothing
   Just ref -> aopt hiddenField "" $ Just (Just (toPathPiece ref))
 where
   containsChar :: Text -> Text -> Bool
   containsChar s mail = isInfixOf s $ T.takeWhile (\c -> c /= '@') mail
   -- Custom email validation
   signupEmailField = check validateEmail emailField
   validateEmail mail
     | mail == "" = Left ("Please provide an email!" :: Text)
     | containsChar "+" mail = Left ("Cannot contain a + symbol!" :: Text)
     | otherwise  = Right mail

-- | Pull in the signup banner widget
signupBannerWidget :: Widget
signupBannerWidget = $(widgetFile "signup-banner")

-- | The signup page without any referrer
getSignupR :: Handler Html
getSignupR = do
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm Nothing
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  defaultLayout $ do
    setTitle "Signup for the competition!"
    $(widgetFile "homepage")

-- | The signup page with a referrer
getReferAFriendR :: Text -> Handler Html
getReferAFriendR referrer = do
  (signupFormWidget, signupFormEnctype) <- generateFormPost $ signupForm (Just referrer)
  -- Set the ultimate destination so we can redirect back correctly later on
  setUltDestCurrent
  defaultLayout $ do
    setTitle "Signup for the competition and help your friend!"
    $(widgetFile "homepage")

-- | Handle the form submission from the signup page and redirect accordingly
postSignupR :: Handler Html
postSignupR = do
  ((result, _), _) <- runFormPost $ signupForm Nothing
  case result of
    FormSuccess res -> do
      let mail = signupEmail res
      -- Check if the mail already exists in the system
      maybeCheckMail <- runDB . getBy $ UniqueEmail mail
      case maybeCheckMail of
        Just _  -> do
          setMessage "You are already signed up!"
          redirectUltDest SignupR
        Nothing -> do
          -- Only add the user if they don't already exist
          referredBy <- maybeReferrer $ signupReferrer res
          -- genRef <- liftIO newStdGen
          -- genDash <- liftIO newStdGen
          -- genAct <- liftIO newStdGen
          referralToken <- generateToken 45
          dashboardToken <- generateToken 30
          activationToken <- generateToken 90
          now <- liftIO getCurrentTime
          -- Insert the new user into the database
          _ <- runDB $ do
             _ <- insert $ User mail referredBy referralToken dashboardToken activationToken False now now
             insert $ Job SendActiviatonMail mail Nothing 0 False now now
          redirect ConfirmSignupR
    FormMissing     -> do
      setMessage "It appears the form data never arrived. Please try submitting again!"
      redirectUltDest SignupR
    FormFailure _   -> do
      setMessage "There was an error with the form, did you enter a valid email?"
      redirectUltDest SignupR
  where
    -- Extract the referrer ID from a referral token
    maybeReferrer referrer = do
      case referrer of
        Nothing -> return Nothing
        Just ref -> do
          maybeRef <- runDB . getBy $ UniqueReferralToken ref
          case maybeRef of
            Nothing -> return Nothing
            Just (Entity referrerId _) -> return $ Just referrerId
    -- Generate a random token
    illegalChars = [':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`']
    replaceChar = fmap $ (\c -> if c `elem` illegalChars then chr (ord c - 8) else c)
    replaceSpace = filter (/= '"')
    generateToken l = do
      gen <- liftIO newStdGen
      let ns = randomRs ('1', 'y') gen
      let randomString = replaceSpace . replaceChar . show $ take l ns
      return . T.pack $ HTTP.urlEncode randomString
