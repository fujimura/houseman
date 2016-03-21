{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman.Logger
  ( newLogger
  , runLogger
  , outputLog
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Monoid
import           Data.Text          (Text)
import           Data.Time
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.IO.Error

import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text

import           Procfile.Types

newLogger :: IO Logger
newLogger = newChan

outputLog :: Logger -> IO ()
outputLog logger = go []
  where
    go cs = do
      (name,l) <- readChan logger
      let (color, cs') = name `lookupOrInsertNewColor` cs
      t <- Text.pack <$> formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
      Text.putStrLn (colorString color (t <> " " <> Text.pack name <> ": ") <> l )
      threadDelay 1000
      go cs'
    lookupOrInsertNewColor :: String -> [(String, Color)] -> (Color, [(String, Color)])
    lookupOrInsertNewColor x xs = case x `lookup` xs of
                                      Just color -> (color,xs)
                                      Nothing -> let color = colors !! length xs
                                                     in (color,(x,color):xs)
    colors :: [Color]
    colors = cycle [32..36]

    colorString :: Color -> Text -> Text
    colorString c x = "\x1b[" <> Text.pack (show c) <> "m" <> x <> "\x1b[0m"

runLogger :: String -> Logger -> Handle -> IO ()
runLogger name logger handle = go
  where
    go = do
      c <- (||) <$> hIsClosed handle <*> hIsEOF' handle
      unless c $ do
        Text.hGetLine handle >>= \l -> writeChan logger (name,l)
        threadDelay 1000
        go
    hIsEOF' h = hIsEOF h `catchIOError`
                  (\e -> if ioeGetErrorType e == HardwareFault then return True else ioError e)
