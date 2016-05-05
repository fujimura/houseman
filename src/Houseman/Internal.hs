{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman.Internal
  ( terminateAndWaitForProcess
  , withAllExit
  , withAnyExit
  , withProcess
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.IO
import           System.Process

import           Data.Streaming.Process (Inherited (..), StreamingProcessHandle,
                                         getStreamingProcessExitCode,
                                         streamingProcess,
                                         streamingProcessHandleRaw,
                                         waitForStreamingProcess)

withAnyExit :: (ExitCode -> Bool) -> [StreamingProcessHandle] -> IO a -> IO a
withAnyExit predicate phs action = go
  where
    go = do
      exits <- catMaybes <$> mapM getStreamingProcessExitCode phs
      if any predicate exits
        then action
        else threadDelay 1000 >> go

withAllExit :: (ExitCode -> Bool) -> [StreamingProcessHandle] -> IO a -> IO a
withAllExit predicate phs action = go
  where
    go = do
      exits <- mapM getStreamingProcessExitCode phs
      if all isJust exits && all predicate (catMaybes exits)
        then action
        else threadDelay 1000 >> go

terminateAndWaitForProcess :: StreamingProcessHandle -> IO ExitCode
terminateAndWaitForProcess ph = do
  let ph' = streamingProcessHandleRaw ph
  b <- getStreamingProcessExitCode ph
  case b of
    Just exitcode -> return exitcode
    Nothing -> do
      terminateProcess ph'
      waitForStreamingProcess ph

withProcess :: CreateProcess -> ((Handle, Handle, StreamingProcessHandle) -> IO a) -> IO a
withProcess p action = bracket before restore action
  where
    before = do
      (Inherited, out :: Handle, err :: Handle, ph) <-
        streamingProcess p { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
      encoding <- mkTextEncoding "utf8"
      forM_ [out,err] $ \h -> do
        hSetBuffering h NoBuffering
        hSetEncoding h encoding
        hSetNewlineMode h universalNewlineMode
      return (out,err,ph)
    restore (out,err,_) = do
      close out
      close err
    close x = do
      c <- hIsClosed x
      unless c $ hClose x
