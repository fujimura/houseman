module Procfile.Types where

-- | Procfile, that consists of a list of `App`s.
type Procfile = [App]

-- | An environmental variable.
type Env = (String, String)

-- | An app in `Procfile`.
data App = App { name :: String
               , cmd  :: String
               } deriving (Eq,Ord,Show)
