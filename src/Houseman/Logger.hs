{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman.Logger
  ( newLogger
  , installLogger
  , runLogger
  , stopLogger
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

runLogger :: Logger -> IO (MVar ())
runLogger logger = do
    m <- newEmptyMVar
    _ <- forkIO $ go [] m
    return m
  where
    go cs m = do
      log' <- readChan logger
      case log' of
        Log (name,l) -> do
          let (color, cs') = name `lookupOrInsertNewColor` cs
          t <- Text.pack <$> formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
          Text.putStrLn (colorString color (t <> " " <> Text.pack name <> ": ") <> l )
          threadDelay 1000
          go cs' m
        LogStop -> putMVar m ()
    lookupOrInsertNewColor :: String -> [(String, Color)] -> (Color, [(String, Color)])
    lookupOrInsertNewColor x xs = case x `lookup` xs of
                                      Just color -> (color,xs)
                                      Nothing -> let color = colors !! length xs
                                                     in (color,(x,color):xs)
    colors :: [Color]
    colors = cycle [32..36]

    colorString :: Color -> Text -> Text
    colorString c x = "\x1b[" <> Text.pack (show c) <> "m" <> x <> "\x1b[0m"

installLogger :: String -> Logger -> Handle -> MVar () -> IO ()
installLogger name logger handle m = go
  where
    go = do
      c <- (||) <$> hIsClosed handle <*> hIsEOF' handle
      if c then putMVar m ()
           else do
             Text.hGetLine handle >>= \l -> writeChan logger (Log (name,l))
             threadDelay 1000
             go
    hIsEOF' h = hIsEOF h `catchIOError`
                  (\e -> if ioeGetErrorType e == HardwareFault then return True else ioError e)

stopLogger :: Logger -> IO ()
stopLogger logger = writeChan logger LogStop
