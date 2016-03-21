module Procfile.Types where

import           Control.Concurrent (Chan)
import           Data.Text          (Text)

type Procfile = [App]
type Env = (String, String)
type Color = Int

type Log = (String, Text)
type Logger = Chan Log

data App = App { name :: String
               , cmd  :: String
               , args :: [String]
               , envs :: [Env]
               } deriving (Eq,Ord,Show)
