
-- |
-- Module for Twitter API and Twitter Authentications
-- and Common functions
module Data.TwiHigh
  ( TweetMessage
  , Url
  , UrlParams
  , TwitterScreenName
  , TwitterAccessTokens (..)
  , TwitterAuth (..)
  , httpsTwitterRequestWithParamsTo
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (newManager, parseRequest, setQueryString, responseBody, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Authenticate.OAuth (signOAuth, OAuth, Credential)
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.ByteString.Char8 as B

type TweetMessage = Text
type Url          = String

-- |
-- Mean url query part .
-- See https://www.stackage.org/haddock/lts-7.8/http-client-0.4.31.1/Network-HTTP-Client.html#v:setQueryString
type UrlParams = [(B.ByteString, Maybe B.ByteString)]

-- | Twitter User ID
type TwitterScreenName = ByteString

-- | For Serialized file
data TwitterAccessTokens = TwitterAccessTokens
  { accessToken       :: ByteString
  , accessTokenSecret :: ByteString
  } deriving (Read)

-- | For Twitter Authentication
data TwitterAuth = TwitterAuth OAuth Credential


-- Fetch get or post request
httpsTwitterRequestWithParamsTo :: TwitterAuth -> Url -> UrlParams -> IO LazyB.ByteString
httpsTwitterRequestWithParamsTo (TwitterAuth oauth credential) url params = do
  manager       <- newManager tlsManagerSettings
  requestModel  <- parseRequest url
  let requestWithParams = setQueryString params requestModel
  signedRequest <- signOAuth oauth credential requestWithParams
  response      <- httpLbs signedRequest manager
  return $ responseBody response
