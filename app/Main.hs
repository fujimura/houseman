{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Maybe
import           Options.Generic
import           Text.Trifecta   (parseFromFile)

import qualified Houseman
import qualified Procfile.Parse

data Option = Start { filename :: Maybe FilePath } |
              Run   { command :: String, filename :: Maybe FilePath }
              deriving (Generic, Show)

instance ParseRecord Option

main :: IO ()
main = do
    (opts :: Option) <- getRecord "Manage Procfile-based application"
    mProcfile <- parseFromFile Procfile.Parse.procfile (fromMaybe "Procfile" $ filename opts)

    case (opts, mProcfile) of
      (_, Nothing)                   -> return ()
      (Start _, Just procfile)       -> Houseman.start procfile
      (Run {command}, Just procfile) -> Houseman.run command procfile
