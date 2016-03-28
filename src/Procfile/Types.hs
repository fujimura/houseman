module Procfile.Types where

type Procfile = [App]
type Env = (String, String)

data App = App { name :: String
               , cmd  :: String
               , args :: [String]
               , envs :: [Env]
               } deriving (Eq,Ord,Show)
