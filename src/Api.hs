{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import Servant
import Servant.API.Stream
import Types

-- | API definition
type API =
  -- Health check
  "health" :> Get '[JSON] Health

  -- Get all media items
  :<|> "api" :> "media" :> Get '[JSON] [MediaItem]

  -- Get specific media item
  :<|> "api" :> "media" :> Capture "id" Int :> Get '[JSON] MediaItem

  -- Stream video
  :<|> "api" :> "stream" :> Capture "id" Int :> QueryParam "quality" Text :> StreamGet NoFraming OctetStream (SourceIO ByteString)

  -- Scan media library
  :<|> "api" :> "scan" :> Post '[JSON] ScanResult

-- | Result of a media scan
data ScanResult = ScanResult
  { scanFound :: Int
  , scanAdded :: Int
  } deriving (Show)

instance ToJSON ScanResult where
  toJSON (ScanResult found added) = object
    [ "found" .= found
    , "added" .= added
    ]

api :: Proxy API
api = Proxy
