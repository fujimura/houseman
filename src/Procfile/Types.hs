module Procfile.Types where

import           Control.Concurrent (Chan, MVar)
import           Data.Text          (Text)

type Procfile = [App]
type Env = (String, String)
type Color = Int

data Log = Log (String, Text) | LogStop deriving (Eq,Ord,Show)
data Logger = Logger (Chan Log) (MVar ())

data App = App { name :: String
               , cmd  :: String
               , args :: [String]
               , envs :: [Env]
               } deriving (Eq,Ord,Show)
