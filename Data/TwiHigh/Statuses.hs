{-# LANGUAGE OverloadedStrings #-}

-- | Module for https://dev.twitter.com/rest/reference/get/statuses/
module Data.TwiHigh.Statuses
  ( UserTimeline
  , UserTimelineItem (..)
  , postTweet
  , fetchUserTimeline
  ) where

import Data.Aeson (FromJSON (..), Value (..), (.:), decode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.TwiHigh


-- |
-- The data type of statuses/user_timeline
-- See https://dev.twitter.com/rest/reference/get/statuses/user_timeline
type UserTimeline = [UserTimelineItem]
data UserTimelineItem = UserTimelineItem
  { userTimelineItemText :: Text
  --TODO: and more fields :P
  } deriving (Show)

instance FromJSON UserTimelineItem where
  parseJSON (Object v) = UserTimelineItem <$> v .: "text" -- and more items :P
  parseJSON _          = error "caught unexpected json field"


-- |
-- Post a tweet
-- and return posted tweet message
--TODO: Return result of success or failure
--postTweet :: TwitterAuth -> TweetMessage -> IO (Maybe Update)
postTweet :: TwitterAuth -> TweetMessage -> IO TweetMessage
postTweet auth message = do
  let urlParams = [("status", Just $ encodeUtf8 message)]
  httpsTwitterRequestWithParamsTo auth "POST https://api.twitter.com/1.1/statuses/update.json" urlParams
  return message
  --decode <$> httpsTwitterRequestWithParamsTo auth "POST https://api.twitter.com/1.1/statuses/update.json" urlParams


-- |
-- Fetch {screenName}'s tweets as Timeline
-- See https://dev.twitter.com/rest/reference/get/statuses/user_timeline
fetchUserTimeline :: TwitterAuth -> TwitterScreenName -> UrlParams -> IO (Maybe UserTimeline)
fetchUserTimeline auth screenName urlParams = do
  let urlParams' = ("screen_name", Just screenName) : urlParams
  decode <$> httpsTwitterRequestWithParamsTo auth "https://api.twitter.com/1.1/statuses/user_timeline.json" urlParams'
