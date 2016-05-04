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
import           Control.Exception        (onException)
import           Control.Monad
import           Data.Char
import           Data.Monoid
import           Data.Text                (Text)
import           Data.Time
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.IO.Error

import qualified Data.ByteString          as ByteString
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO             as Text

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

installLogger :: String -> Logger -> Handle -> IO ()
installLogger name (Logger logger _) handle = go
  where
    go = do
      c <- (||) <$> hIsClosed handle <*> hIsEOF' handle
      unless c $ do
        l <- Text.decodeUtf8With Text.lenientDecode . ByteString.filter (/= fromIntegral (ord '\r'))
             <$> ByteString.hGetLine handle
        writeChan logger (Log (name,l))
        go
    hIsEOF' h = hIsEOF h `catchIOError`
                  (\e -> if ioeGetErrorType e == HardwareFault then return True else ioError e)

stopLogger :: Logger -> IO ()
stopLogger (Logger logger stop) = do
    -- Wait a while to flush logs
    -- FIXME This won't guarantee all logs will be flushed out.
    threadDelay 1000
    writeChan logger LogStop
    takeMVar stop
