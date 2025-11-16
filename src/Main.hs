{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import Database
import Server
import Types

-- | Load configuration from environment variables
loadConfig :: IO Config
loadConfig = do
  dbHost <- getEnvDefault "DB_HOST" "localhost"
  dbPort <- read <$> getEnvDefault "DB_PORT" "5432"
  dbUser <- getEnvDefault "DB_USER" "jellyfin"
  dbPassword <- getEnvDefault "DB_PASSWORD" "jellyfin"
  dbName <- getEnvDefault "DB_NAME" "jellyfin"
  serverPort <- read <$> getEnvDefault "SERVER_PORT" "8080"
  mediaRoot <- getEnvDefault "MEDIA_ROOT" "/media"

  return Config
    { configDbHost = dbHost
    , configDbPort = dbPort
    , configDbUser = dbUser
    , configDbPassword = dbPassword
    , configDbName = dbName
    , configServerPort = serverPort
    , configMediaRoot = mediaRoot
    }

-- | Get environment variable with default value
getEnvDefault :: String -> String -> IO String
getEnvDefault key defaultValue = do
  maybeValue <- lookupEnv key
  return $ maybe defaultValue id maybeValue

main :: IO ()
main = do
  hPutStrLn stderr "Starting Jellyfin-HS server..."

  -- Load configuration
  config <- loadConfig
  hPutStrLn stderr $ "Server port: " ++ show (configServerPort config)
  hPutStrLn stderr $ "Media root: " ++ configMediaRoot config
  hPutStrLn stderr $ "Database: " ++ configDbHost config ++ ":" ++ show (configDbPort config)

  -- Create database connection pool
  pool <- createPool config
  hPutStrLn stderr "Database pool created"

  -- Initialize database schema
  withResource pool initDb
  hPutStrLn stderr "Database schema initialized"

  -- Start the server
  hPutStrLn stderr $ "Server listening on http://localhost:" ++ show (configServerPort config)
  run (configServerPort config) (app config pool)
