{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman where

import           Control.Concurrent
import           Control.Monad
import           Data.List
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Posix.Signals
import           System.Process

import qualified Configuration.Dotenv as Dotenv

import           Houseman.Internal    (bracketMany, runInPseudoTerminal,
                                       terminateAll, terminateAndWaitForProcess,
                                       watchTerminationOfProcesses)
import           Houseman.Logger      (newLogger, outputLog, runLogger)
import           Procfile.Types

run :: String -> Procfile -> IO ()
run cmd' apps = case find (\App{cmd} -> cmd == cmd') apps of
                  Just app -> Houseman.start [app] -- TODO Remove color in run command
                  Nothing   -> die ("Command '" ++ cmd' ++ "' not found in Procfile")

start :: [App] -> IO ()
start apps = do
    print apps

    -- Allocate logger
    logger <- newLogger

    -- Run apps
    bracketMany (map (`runApp` logger) apps) terminateAndWaitForProcess $ \phs -> do
      -- Get a MVar for exit code
      m <- newEmptyMVar

      -- Kill apps with signals
      [sigINT, sigTERM, sigKILL, keyboardSignal] `forM_` \signal ->
        installHandler signal (Catch (terminateAll m phs)) Nothing

      -- If an app was terminated, terminate others as well
      _ <- forkIO $ watchTerminationOfProcesses (terminateAll m phs) phs

      -- Output logs to stdout
      _ <- forkIO $ outputLog logger

      -- Wait MVar is filled with exit code. It will be filled in keyboard
      -- handler or termination of an app
      exitStatus <- takeMVar m
      putStrLn "bye"
      exitWith exitStatus

-- Run given app with given logger.
runApp :: App -> Logger -> IO ProcessHandle
runApp App {name,cmd,args,envs} logger =  do
    -- Build environment variables to run app.
    -- Priority: 1. Procfile, 2. dotenv, 3. the environment
    envs' <- nub . mconcat <$> sequence [return envs, getEnvsInDotEnvFile,  getEnvironment]
    (master, _, ph) <- runInPseudoTerminal (proc cmd args) { env = Just envs' }
    _ <- forkIO $ runLogger name logger master
    return ph
  where
    getEnvsInDotEnvFile :: IO [Env]
    getEnvsInDotEnvFile = do
        d <- doesFileExist ".env"
        if d then Dotenv.parseFile ".env" else return []
