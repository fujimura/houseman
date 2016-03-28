{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman.Logger
  ( newLogger
  , installLogger
  , runLogger
  , readLogger
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

import           Houseman.Types

newLogger :: IO Logger
newLogger = do
    c <- newChan
    m <- newEmptyMVar
    return $ Logger c m

readLogger :: Logger -> IO Log
readLogger (Logger logger _) = readChan logger

runLogger :: Logger -> IO ()
runLogger (Logger logger done) = do
    _ <- forkIO $ go []
    return ()
  where
    go cs = do
      log' <- readChan logger
      case log' of
        Log (name,l) -> do
          let (color, cs') = name `lookupOrInsertNewColor` cs
          t <- Text.pack <$> formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
          Text.putStrLn (colorString color (t <> " " <> Text.pack name <> ": ") <> l )
          threadDelay 1000
          go cs'
        LogStop -> putMVar done ()
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
installLogger name (Logger logger _) handle m = go
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
stopLogger (Logger logger stop) = do
    writeChan logger LogStop
    takeMVar stop
