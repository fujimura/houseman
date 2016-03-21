{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman.Internal
  ( watchTerminationOfProcesses
  , terminateAll
  , terminateAndWaitForProcess
  , fdsToHandles
  , runInPseudoTerminal
  , bracketMany
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

import qualified System.Posix.IO       as IO
import qualified System.Posix.Terminal as Terminal

watchTerminationOfProcesses :: IO a -> [ProcessHandle] -> IO ()
watchTerminationOfProcesses handler phs = go
  where
    go = do
      b <- any isJust <$> mapM getProcessExitCode phs
      if b then void handler
           else do
             threadDelay 1000
             go

terminateAll :: MVar ExitCode -> [ProcessHandle] -> IO ()
terminateAll m phs = do
  forM_ phs terminateAndWaitForProcess
  putMVar m (ExitFailure 1)

terminateAndWaitForProcess :: ProcessHandle -> IO ExitCode
terminateAndWaitForProcess ph = do
  b <- getProcessExitCode ph
  case b of
    Just exitcode -> return exitcode
    Nothing -> do
      terminateProcess ph
      waitForProcess ph

fdsToHandles :: (Fd, Fd) -> IO (Handle, Handle)
fdsToHandles (x, y) = (,) <$> IO.fdToHandle x <*> IO.fdToHandle y

runInPseudoTerminal :: CreateProcess -> IO (Handle, Handle, ProcessHandle)
runInPseudoTerminal p = do
    -- TODO handle leaks possibility
    (read',write) <- fdsToHandles =<< IO.createPipe
    (master,slave) <- fdsToHandles =<< Terminal.openPseudoTerminal
    encoding <- mkTextEncoding "utf8"
    forM_ [read',write,master,slave,stdout] $ \h -> do
      hSetBuffering h NoBuffering
      hSetEncoding h encoding
    hSetNewlineMode master universalNewlineMode
    (_, _, _, ph) <-
        createProcess p { std_in = UseHandle read'
                        , std_out = UseHandle slave
                        , std_err = UseHandle slave
                        }

    return (master, write, ph)

bracketMany :: [IO a] -> (a -> IO b) -> ([a] -> IO c) -> IO c
bracketMany = go []
  where
    go :: [a] -> [IO a] -> (a -> IO b) -> ([a] -> IO c) -> IO c
    go cs []               _     thing = thing cs
    go cs (before:befores) after thing = bracket before after (\c -> go (c:cs) befores after thing)
