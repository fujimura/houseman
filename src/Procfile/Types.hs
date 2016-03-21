module Procfile.Types where

type Procfile = [App]
type Env = (String, String)
type Color = Int

data App = App { name :: String
               , cmd  :: String
               , args :: [String]
               , envs :: [Env]
               } deriving (Eq,Ord,Show)
