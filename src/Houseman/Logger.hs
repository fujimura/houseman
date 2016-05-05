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
import           Data.Char
import           Data.Monoid
import           Data.Text                (Text)
import           Data.Time
import           GHC.IO.Handle

import qualified Data.ByteString          as ByteString
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO             as Text
import qualified System.IO.Streams        as Streams

import           Houseman.Types

-- | Instantiates new 'Logger'.
newLogger :: IO Logger
newLogger = do
    c <- newChan
    m <- newEmptyMVar
    return $ Logger c m

-- | Reads one `Log` from given `Logger`.
readLogger :: Logger -> IO Log
readLogger (Logger logger _) = readChan logger

-- | Runs given `Logger`. Logs will be output to `System.IO.stdout`. When `Logger` gets
-- `LogStop`, logging will be stopped.
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

-- | Installs new `Handle` as a source of given `Logger`.
installLogger
  :: String -- ^ Name of source
  -> Logger -- ^ `Logger` instance
  -> Handle -- ^ The source
  -> IO ()
installLogger name (Logger logger _) handle = do
    is <- Streams.handleToInputStream handle >>=
      Streams.lines >>=
      Streams.map (Text.decodeUtf8With Text.lenientDecode . ByteString.filter (/= fromIntegral (ord '\r')))
    os <- Streams.makeOutputStream out
    Streams.connect is os
  where
    out (Just l) = writeChan logger (Log (name,l))
    out Nothing  = return ()

-- | Stops `Logger`.
stopLogger :: Logger -> IO ()
stopLogger (Logger logger stop) = do
    -- Wait a while to flush logs
    -- FIXME This won't guarantee all logs will be flushed out.
    threadDelay 1000
    writeChan logger LogStop
    takeMVar stop
