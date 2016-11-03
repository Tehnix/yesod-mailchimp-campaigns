module Job.Common
  ( mailchimpPostRequest
  , mailchimpPatchRequest
  ) where

import           Import

import qualified Crypto.Hash         as CH
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Network.HTTP.Simple as HTTP

hexMD5 :: Text -> String
hexMD5 s = show (CH.hash (T.encodeUtf8 s) :: CH.Digest CH.MD5)

mailchimpPostRequest :: ToJSON a => App -> Language -> a -> Request
mailchimpPostRequest master lang body = do
  let url = mailchimpEndpoint master lang "POST" ""
  mailchimpRequest master body url

mailchimpPatchRequest :: ToJSON a => App -> Language -> a -> Text -> Request
mailchimpPatchRequest master lang body mail = do
  let url = mailchimpEndpoint master lang "PATCH" $ hexMD5 mail
  mailchimpRequest master body url

mailchimpEndpoint :: App -> Language -> String -> String -> Request
mailchimpEndpoint master lang httpType extra = do
  let mailchimpApiLocation = mcApiLocation . appMailchimp $ appSettings master
  let mailchimpListId = case lang of
        Danish    -> mcListIdDanish . mcListId . appMailchimp $ appSettings master
        Swedish   -> mcListIdSwedish . mcListId . appMailchimp $ appSettings master
        Norwegian -> mcListIdNorwegian . mcListId . appMailchimp $ appSettings master
  let mailchimpApiEndpoint = T.unpack $ "http://" <> mailchimpApiLocation <> ".api.mailchimp.com/3.0/lists/" <> mailchimpListId <> "/members/"
  parseRequest_ $ httpType <> " " <> mailchimpApiEndpoint <> extra

mailchimpRequest :: ToJSON a => App -> a -> Request -> Request
mailchimpRequest master body url = do
  let mailchimpApiUser = T.encodeUtf8 . mcApiUser . appMailchimp $ appSettings master
  let mailchimpApiKey = T.encodeUtf8 . mcApiKey . appMailchimp $ appSettings master
  HTTP.setRequestBasicAuth mailchimpApiUser mailchimpApiKey . HTTP.setRequestIgnoreStatus $ HTTP.setRequestBodyJSON body url
