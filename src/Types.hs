{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

-- | Media item stored in the database
data MediaItem = MediaItem
  { mediaId :: Int
  , mediaPath :: FilePath
  , mediaTitle :: Text
  , mediaDuration :: Maybe Double
  , mediaType :: MediaType
  , mediaCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON MediaItem
instance FromJSON MediaItem

-- | Type of media
data MediaType
  = Video
  | Audio
  | Image
  deriving (Show, Eq, Generic)

instance ToJSON MediaType
instance FromJSON MediaType

-- | Video stream quality preset
data Quality
  = Original
  | High      -- 1080p
  | Medium    -- 720p
  | Low       -- 480p
  deriving (Show, Eq)

-- | Server configuration
data Config = Config
  { configDbHost :: String
  , configDbPort :: Int
  , configDbUser :: String
  , configDbPassword :: String
  , configDbName :: String
  , configServerPort :: Int
  , configMediaRoot :: FilePath
  } deriving (Show)

-- | Health check response
data Health = Health
  { healthStatus :: Text
  , healthDatabase :: Text
  } deriving (Show, Generic)

instance ToJSON Health
