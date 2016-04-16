{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman.Internal
  ( terminateAndWaitForProcess
  , withAllExit
  , withAnyExit
  , runInPseudoTerminal
  , bracketOnErrorMany
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.IO
import           System.Posix.Types
import           System.Process

import           Data.Streaming.Process (StreamingProcessHandle,
                                         streamingProcess,
                                         waitForStreamingProcess,
                                         getStreamingProcessExitCode,
                                         UseProvidedHandle(..),
                                         streamingProcessHandleRaw)

import qualified System.Posix.IO        as IO
import qualified System.Posix.Terminal  as Terminal


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

fdsToHandles :: (Fd, Fd) -> IO (Handle, Handle)
fdsToHandles (x, y) = (,) <$> IO.fdToHandle x <*> IO.fdToHandle y

runInPseudoTerminal :: CreateProcess -> IO (Handle, Handle, StreamingProcessHandle)
runInPseudoTerminal p = do
    -- TODO handle leaks possibility
    (read',write) <- fdsToHandles =<< IO.createPipe
    (master,slave) <- fdsToHandles =<< Terminal.openPseudoTerminal
    encoding <- mkTextEncoding "utf8"
    forM_ [read',write,master,slave,stdout] $ \h -> do
      hSetBuffering h NoBuffering
      hSetEncoding h encoding
    hSetNewlineMode master universalNewlineMode
    (UseProvidedHandle, UseProvidedHandle, UseProvidedHandle, ph) <-
      streamingProcess p { std_in = UseHandle read'
                         , std_out = UseHandle slave
                         , std_err = UseHandle slave
                         }
    return (master, write, ph)

bracketOnErrorMany :: [IO a] -> (a -> IO b) -> ([a] -> IO c) -> IO c
bracketOnErrorMany = go []
  where
    go :: [a] -> [IO a] -> (a -> IO b) -> ([a] -> IO c) -> IO c
    go cs []               _     thing = thing cs
    go cs (before:befores) after thing = bracketOnError before after (\c -> go (c:cs) befores after thing)
