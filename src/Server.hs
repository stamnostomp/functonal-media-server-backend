{-# LANGUAGE OverloadedStrings #-}

module Server (app, mkApp) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Middleware.Cors
import Servant
import Servant.API.Stream

import Api
import Database
import Media.FFmpeg
import Media.Scanner
import Types

-- | Application state
data AppState = AppState
  { appConfig :: Config
  , appDbPool :: Pool Connection
  }

-- | Create the WAI application
mkApp :: Config -> Pool Connection -> Application
mkApp config pool =
  let state = AppState config pool
  in cors (const $ Just corsPolicy) $ serve api (server state)

-- | CORS policy to allow web clients
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  }

-- | API server implementation
server :: AppState -> Server API
server state =
  healthHandler state
  :<|> getMediaHandler state
  :<|> getMediaByIdHandler state
  :<|> streamHandler state
  :<|> scanHandler state

-- | Health check handler
healthHandler :: AppState -> Handler Health
healthHandler state = liftIO $ do
  -- Try to connect to database
  dbStatus <- checkDatabase (appDbPool state)
  return $ Health
    { healthStatus = "ok"
    , healthDatabase = if dbStatus then "connected" else "disconnected"
    }

-- | Check database connection
checkDatabase :: Pool Connection -> IO Bool
checkDatabase pool = do
  result <- try $ withResource pool $ \conn ->
    query_ conn "SELECT 1" :: IO (Either SomeException [[Int]])
  case result of
    Right _ -> return True
    Left _ -> return False
  where
    try :: IO a -> IO (Either SomeException a)
    try = Control.Exception.try

-- | Get all media items
getMediaHandler :: AppState -> Handler [MediaItem]
getMediaHandler state = liftIO $ getMediaItems (appDbPool state)

-- | Get media item by ID
getMediaByIdHandler :: AppState -> Int -> Handler MediaItem
getMediaByIdHandler state mediaId = do
  maybeItem <- liftIO $ getMediaById (appDbPool state) mediaId
  case maybeItem of
    Just item -> return item
    Nothing -> throwError err404 { errBody = "Media item not found" }

-- | Stream video handler
streamHandler :: AppState -> Int -> Maybe T.Text -> Handler (SourceIO ByteString)
streamHandler state mediaId qualityParam = do
  -- Get media item from database
  maybeItem <- liftIO $ getMediaById (appDbPool state) mediaId
  case maybeItem of
    Nothing -> throwError err404 { errBody = "Media item not found" }
    Just item -> do
      let quality = parseQuality qualityParam
      -- Stream the video using ffmpeg
      return $ streamVideo (mediaPath item) quality

-- | Parse quality parameter
parseQuality :: Maybe T.Text -> Quality
parseQuality Nothing = Original
parseQuality (Just q) = case T.toLower q of
  "high" -> High
  "medium" -> Medium
  "low" -> Low
  _ -> Original

-- | Scan media library handler
scanHandler :: AppState -> Handler ScanResult
scanHandler state = liftIO $ do
  let mediaRoot = configMediaRoot (appConfig state)

  -- Scan directory for video files
  videoFiles <- scanDirectory mediaRoot

  -- Insert new videos into database
  added <- foldM (insertIfNew (appDbPool state)) 0 videoFiles

  return $ ScanResult
    { scanFound = length videoFiles
    , scanAdded = added
    }

-- | Insert video if it doesn't exist
insertIfNew :: Pool Connection -> Int -> FilePath -> IO Int
insertIfNew pool count videoPath = do
  -- Try to get video duration
  duration <- getVideoInfo videoPath

  -- Try to insert
  result <- insertMediaItem pool videoPath (extractTitle videoPath) duration Video

  case result of
    Just _ -> return (count + 1)
    Nothing -> return count  -- Already exists or failed

-- Required imports for exception handling
import Control.Exception (SomeException)
import qualified Control.Exception
import Control.Monad (foldM)

app :: Config -> Pool Connection -> Application
app = mkApp
