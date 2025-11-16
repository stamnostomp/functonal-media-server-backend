{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initDb
  , createPool
  , getMediaItems
  , insertMediaItem
  , getMediaById
  ) where

import Control.Exception (bracket)
import Data.Pool
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Types

-- | Initialize database schema
initDb :: Connection -> IO ()
initDb conn = do
  execute_ conn $ mconcat
    [ "CREATE TABLE IF NOT EXISTS media_items ("
    , "  id SERIAL PRIMARY KEY,"
    , "  path TEXT NOT NULL UNIQUE,"
    , "  title TEXT NOT NULL,"
    , "  duration DOUBLE PRECISION,"
    , "  media_type TEXT NOT NULL,"
    , "  created_at TIMESTAMP NOT NULL DEFAULT NOW()"
    , ")"
    ]
  return ()

-- | Create connection pool
createPool :: Config -> IO (Pool Connection)
createPool config =
  newPool $ defaultPoolConfig
    (connect (ConnectInfo
      (configDbHost config)
      (fromIntegral $ configDbPort config)
      (configDbUser config)
      (configDbPassword config)
      (configDbName config)))
    close
    10  -- stripe count
    60  -- keep alive time

-- | FromRow instance for MediaItem
instance FromRow MediaItem where
  fromRow = MediaItem
    <$> field  -- id
    <$> field  -- path
    <$> field  -- title
    <$> field  -- duration
    <$> (parseMediaType <$> field)  -- media_type
    <$> field  -- created_at

-- | Parse media type from text
parseMediaType :: String -> MediaType
parseMediaType "video" = Video
parseMediaType "audio" = Audio
parseMediaType "image" = Image
parseMediaType _ = Video

-- | Serialize media type
serializeMediaType :: MediaType -> String
serializeMediaType Video = "video"
serializeMediaType Audio = "audio"
serializeMediaType Image = "image"

-- | Get all media items
getMediaItems :: Pool Connection -> IO [MediaItem]
getMediaItems pool = withResource pool $ \conn ->
  query_ conn "SELECT id, path, title, duration, media_type, created_at FROM media_items ORDER BY created_at DESC"

-- | Insert a new media item
insertMediaItem :: Pool Connection -> FilePath -> String -> Maybe Double -> MediaType -> IO (Maybe Int)
insertMediaItem pool path title duration mediaType = withResource pool $ \conn -> do
  result <- query conn
    "INSERT INTO media_items (path, title, duration, media_type) VALUES (?, ?, ?, ?) RETURNING id"
    (path, title, duration, serializeMediaType mediaType)
  case result of
    [Only itemId] -> return $ Just itemId
    _ -> return Nothing

-- | Get media item by ID
getMediaById :: Pool Connection -> Int -> IO (Maybe MediaItem)
getMediaById pool mediaId = withResource pool $ \conn -> do
  result <- query conn
    "SELECT id, path, title, duration, media_type, created_at FROM media_items WHERE id = ?"
    (Only mediaId)
  case result of
    [item] -> return $ Just item
    _ -> return Nothing
