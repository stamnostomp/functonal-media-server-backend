{-# LANGUAGE OverloadedStrings #-}

module Media.Scanner
  ( scanDirectory
  , isVideoFile
  , extractTitle
  ) where

import Control.Monad (filterM, forM)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Types

-- | Video file extensions to look for
videoExtensions :: [String]
videoExtensions = [".mp4", ".mkv", ".avi", ".mov", ".wmv", ".flv", ".webm", ".m4v"]

-- | Check if a file is a video based on extension
isVideoFile :: FilePath -> Bool
isVideoFile path = any (`isSuffixOf` lowerPath) videoExtensions
  where
    lowerPath = map toLower path

-- | Extract a title from a filename
extractTitle :: FilePath -> String
extractTitle path =
  let filename = takeBaseName path
      -- Replace common separators with spaces
      cleaned = map (\c -> if c `elem` ("._-" :: String) then ' ' else c) filename
  in cleaned

-- | Recursively scan a directory for video files
scanDirectory :: FilePath -> IO [FilePath]
scanDirectory dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries

      -- Separate files and directories
      files <- filterM doesFileExist fullPaths
      dirs <- filterM doesDirectoryExist fullPaths

      -- Filter video files
      let videoFiles = filter isVideoFile files

      -- Recursively scan subdirectories
      subVideos <- concat <$> mapM scanDirectory dirs

      return $ videoFiles ++ subVideos
