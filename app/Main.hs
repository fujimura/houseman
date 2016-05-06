{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Maybe
import           Data.Version    (showVersion)
import           Options.Generic
import           System.Exit
import           Text.Trifecta   (parseFromFile)

import qualified Houseman
import qualified Paths_houseman
import qualified Procfile.Parse
import           Procfile.Types

data Option = Start { filename :: Maybe FilePath } |
              Run   { command :: String, filename :: Maybe FilePath } |
              Version
              deriving (Generic, Show)

instance ParseRecord Option

main :: IO ()
main = do
    (opts :: Option) <- getRecord "Manage Procfile-based application"
    case opts of
      Start _       -> parseProcFile (fromMaybe "Procfile" $ filename opts) >>= Houseman.start >>= exitWith
      Run {command} -> parseProcFile (fromMaybe "Procfile" $ filename opts) >>= Houseman.run command >>= exitWith
      Version       -> putStrLn (showVersion Paths_houseman.version)

parseProcFile :: FilePath -> IO Procfile
parseProcFile path = do
  mProcfile <- parseFromFile Procfile.Parse.procfile path
  case mProcfile of
    Nothing -> exitWith (ExitFailure 1)
    Just p -> return p
