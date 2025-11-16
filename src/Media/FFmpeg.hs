{-# LANGUAGE OverloadedStrings #-}

module Media.FFmpeg
  ( streamVideo
  , getVideoInfo
  , transcodeVideo
  , qualityToResolution
  ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Process
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Process
import Types

-- | Get quality resolution parameters for ffmpeg
qualityToResolution :: Quality -> [String]
qualityToResolution Original = []
qualityToResolution High = ["-vf", "scale=-2:1080"]
qualityToResolution Medium = ["-vf", "scale=-2:720"]
qualityToResolution Low = ["-vf", "scale=-2:480"]

-- | Stream video file with optional transcoding
-- Returns a conduit that produces video chunks
streamVideo :: FilePath -> Quality -> ConduitT () ByteString IO ()
streamVideo videoPath quality = do
  let ffmpegArgs = concat
        [ ["-i", videoPath]
        , qualityToResolution quality
        , [ "-c:v", "libx264"
          , "-preset", "veryfast"
          , "-c:a", "aac"
          , "-f", "matroska"  -- Use matroska container for streaming
          , "-"  -- Output to stdout
          ]
        ]

  -- Create the ffmpeg process
  sourceCmdWithStreams "ffmpeg" ffmpegArgs

-- | Source from a command that writes to stdout
sourceCmdWithStreams :: String -> [String] -> ConduitT () ByteString IO ()
sourceCmdWithStreams cmd args = do
  let cp = (proc cmd args)
        { std_out = CreatePipe
        , std_err = Inherit
        }

  bracketP
    (createProcess cp)
    (\(_, mOut, _, ph) -> do
      maybe (return ()) hClose mOut
      void $ waitForProcess ph)
    (\(_, mOut, _, _) ->
      case mOut of
        Nothing -> return ()
        Just hOut -> sourceHandle hOut)

-- | Read from a handle and yield chunks
sourceHandle :: Handle -> ConduitT () ByteString IO ()
sourceHandle h = do
  chunk <- liftIO $ BS.hGetSome h (64 * 1024)  -- 64KB chunks
  if BS.null chunk
    then return ()
    else yield chunk >> sourceHandle h

-- | Transcode video to a specific quality and save to file
transcodeVideo :: FilePath -> FilePath -> Quality -> IO ExitCode
transcodeVideo inputPath outputPath quality = do
  let ffmpegArgs = concat
        [ ["-i", inputPath]
        , qualityToResolution quality
        , [ "-c:v", "libx264"
          , "-preset", "medium"
          , "-c:a", "aac"
          , "-movflags", "+faststart"
          , outputPath
          ]
        ]

  (_, _, _, ph) <- createProcess (proc "ffmpeg" ffmpegArgs)
  waitForProcess ph

-- | Get video information using ffprobe
getVideoInfo :: FilePath -> IO (Maybe Double)
getVideoInfo videoPath = do
  let ffprobeArgs =
        [ "-v", "error"
        , "-show_entries", "format=duration"
        , "-of", "default=noprint_wrappers=1:nokey=1"
        , videoPath
        ]

  (_, Just hOut, _, ph) <- createProcess (proc "ffprobe" ffprobeArgs)
    { std_out = CreatePipe }

  output <- hGetContents hOut
  exitCode <- waitForProcess ph

  case exitCode of
    ExitSuccess -> return $ readMaybe output
    _ -> return Nothing
  where
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing
