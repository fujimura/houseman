{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Signals
import           System.Posix.Types
import           System.Process

import qualified Configuration.Dotenv  as Dotenv
import qualified System.Posix.IO       as IO
import qualified System.Posix.Terminal as Terminal

import           Houseman.Logger       (newLogger, outputLog, runLogger)
import           Procfile.Types

run :: String -> Procfile -> IO ()
run cmd' apps = case find (\App{cmd} -> cmd == cmd') apps of
                  Just proc -> Houseman.start [proc] -- TODO Remove color in run command
                  Nothing   -> die ("Command '" ++ cmd' ++ "' not found in Procfile")

start :: [App] -> IO ()
start apps = do
    print apps
    logger <- newLogger
    bracketMany (map (flip runApp logger) apps) terminateAndWaitForProcess $ \phs -> do
      m <- newEmptyMVar

      installHandler keyboardSignal (Catch (terminateAll m phs)) Nothing

      _ <- forkIO $ waitForProcessesAndTerminateAll m phs
      _ <- forkIO $ outputLog logger

      exitStatus <- takeMVar m
      putStrLn "bye"
      exitWith exitStatus
  where
    waitForProcessesAndTerminateAll :: MVar ExitCode -> [ProcessHandle] -> IO ()
    waitForProcessesAndTerminateAll m phs = go
      where
        go = do
          b <- any isJust <$> mapM getProcessExitCode phs
          when b $ terminateAll m phs
          threadDelay 1000
          go

    terminateAll :: MVar ExitCode -> [ProcessHandle] -> IO ()
    terminateAll m phs = do
      forM_ phs terminateAndWaitForProcess
      putMVar m (ExitFailure 1)

terminateAndWaitForProcess :: ProcessHandle -> IO ()
terminateAndWaitForProcess ph = do
  b <- getProcessExitCode ph
  when (isNothing b) $ do
    terminateProcess ph
    waitForProcess ph
    return ()

runApp :: App -> Logger -> IO ProcessHandle
runApp App {name,cmd,args,envs} logger =  do
    currentEnvs <- getEnvironment
    d <- doesFileExist ".env"
    dotEnvs <- if d then Dotenv.parseFile ".env" else return []
    (master, _, ph) <- runInPseudoTerminal (proc cmd args) { env = Just (nub $ envs ++ dotEnvs ++ currentEnvs)}
    forkIO $ runLogger name logger master
    return ph

fdsToHandles :: (Fd, Fd) -> IO (Handle, Handle)
fdsToHandles (x, y) = (,) <$> IO.fdToHandle x <*> IO.fdToHandle y

runInPseudoTerminal :: CreateProcess -> IO (Handle, Handle, ProcessHandle)
runInPseudoTerminal p = do
    -- TODO handle leaks possibility
    (read,write) <- fdsToHandles =<< IO.createPipe
    (master,slave) <- fdsToHandles =<< Terminal.openPseudoTerminal
    encoding <- mkTextEncoding "utf8"
    forM_ [read,write,master,slave,stdout] $ \handle -> do
      hSetBuffering handle NoBuffering
      hSetEncoding handle encoding
    hSetNewlineMode master universalNewlineMode
    (_, _, _, ph) <-
        createProcess p { std_in = UseHandle read
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
