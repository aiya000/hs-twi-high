{-# LANGUAGE OverloadedStrings #-}

-- | Module for https://dev.twitter.com/rest/reference/get/followers/{some}
module Data.TwiHigh.Followers
  ( List (..)
  , ListUsers (..)
  ) where

import Data.Aeson (FromJSON (..), Value (..), (.:), decode)
import Data.Text (Text)
import Data.TwiHigh


---- See https://dev.twitter.com/rest/reference/get/followers/ids
--data Ids = Ids
--  { idsIds               :: [Integer]
--  , idsNextCursor        :: Integer
--  , idsNextCursorStr     :: String
--  , idsPreviousCursor    :: Integer
--  , idsPreviousCursorStr :: String
--  } deriving (Show)
--instance FromJSON Ids where
--  parseJSON (Object v) =
--    Ids <$> v .: "ids"
--        <*> v .: "next_cursor"
--        <*> v .: "next_cursor_str"
--        <*> v .: "previous_cursor"
--        <*> v .: "previous_cursor_str"
--  parseJSON _ = error "caught unexpected json field"


-- | See https://dev.twitter.com/rest/reference/get/followers/list
data List = List
  { listUsers             :: [ListUsers]
  , listNextCursor        :: Integer
  , listNextCursorStr     :: String
  , listPreviousCursor    :: Integer
  , listPreviousCursorStr :: String
  } deriving (Show)

instance FromJSON List where
  parseJSON (Object v) =
    List <$> v .: "users"
         <*> v .: "next_cursor"
         <*> v .: "next_cursor_str"
         <*> v .: "previous_cursor"
         <*> v .: "previous_cursor_str"
  parseJSON _ = error "caught unexpected json field"


-- | An item for List
data ListUsers = ListUsers
  { listUsersId              :: Integer
  , listUsersIdStr           :: String
  , listUsersName            :: Text
  , listUsersScreenName      :: String
  --, listUsersLocation        :: ???
  --, listUsersUrl             :: ???
  , listUsersDescription     :: String
  , listUsersProtected       :: Bool
  , listUsersFollowersCount  :: Integer
  , listUsersFriendsCount    :: Integer
  , listUsersListedCount     :: Integer
  , listUsersCreatedAt       :: String
  , listUsersFavouritesCount :: Integer
  --TODO: and more fields :P
  } deriving (Show)

instance FromJSON ListUsers where
  parseJSON (Object v) =
    ListUsers <$> v .: "id"
              <*> v .: "id_str"
              <*> v .: "name"
              <*> v .: "screen_name"
              -- <*> v .: "location"
              -- <*> v .: "url"
              <*> v .: "description"
              <*> v .: "protected"
              <*> v .: "followers_count"
              <*> v .: "friends_count"
              <*> v .: "listed_count"
              <*> v .: "created_at"
              <*> v .: "favourites_count"
              --TODO: and more items :P
  parseJSON _ = error "caught unexpected json field"


-- | Fetch {screenName}'s followers list
-- NOTE: Implement with cursor parameter
fetchFollowersList :: TwitterAuth -> TwitterScreenName -> IO (Maybe List)
fetchFollowersList auth screenName = do
  let urlParams = [("screen_name", Just screenName)]
  decode <$> httpsTwitterRequestWithParamsTo auth "https://api.twitter.com/1.1/followers/list.json" urlParams
