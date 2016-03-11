module Procfile.Types where

type Procfile = [Proc]
type Env = (String, String)
type Color = Int

data Proc = Proc { name :: String
                 , cmd  :: String
                 , args :: [String]
                 , envs :: [Env]
                 } deriving (Eq,Ord,Show)
