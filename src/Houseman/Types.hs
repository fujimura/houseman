module Houseman.Types where

import           Control.Concurrent (Chan, MVar)
import           Data.Text          (Text)

data Log = Log (String, Text) | LogStop deriving (Eq,Ord,Show)
data Logger = Logger (Chan Log) (MVar ())
type Color = Int
