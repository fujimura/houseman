module Houseman.Types where

import           Control.Concurrent (Chan, MVar)
import           Data.Text          (Text)

data Log =
    Log (String, Text) -- ^ Name and log itself
    | LogStop -- ^ To stop log
    deriving (Eq,Ord,Show)

data Logger =
    Logger { logs :: Chan Log -- ^ A channel to store logs
           , done :: MVar () -- ^ Filled when logging is finished
           }

type Color = Int -- Color of log
