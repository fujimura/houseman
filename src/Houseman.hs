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
                                       terminateAndWaitForProcess,
                                       withAnyExit,
                                       withAllExit)
import           Houseman.Logger      (newLogger, runLogger, installLogger, stopLogger)
import           Procfile.Types

run :: String -> Procfile -> IO ExitCode
run cmd' apps = case find (\App{cmd} -> cmd == cmd') apps of
                  Just app -> Houseman.start [app] -- TODO Remove color in run command
                  Nothing   -> die ("Command '" ++ cmd' ++ "' not found in Procfile")

start :: [App] -> IO ExitCode
start apps = do
    print apps

    -- Allocate logger
    logger <- newLogger

    -- Run apps
    bracketMany (map (runApp logger) apps) (terminateAndWaitForProcess . fst) $ \xs -> do
      let phs = map fst xs
          logFinishes = map snd xs
      -- Get a MVar to detect termination of a process
      readyToTerminate <- newEmptyMVar

      -- Output logs to stdout
      logFinished <- runLogger logger

      -- Fill MVar with signal
      [sigINT, sigTERM, keyboardSignal] `forM_` \signal ->
        installHandler signal (Catch (putMVar readyToTerminate ())) Nothing

      -- Fill MVar with any failure
      _ <- forkIO $ withAnyExit (/= ExitSuccess) phs (putMVar readyToTerminate ())
      -- Fill MVar with all success
      _ <- forkIO $ withAllExit (== ExitSuccess) phs (putMVar readyToTerminate ())

      -- Wait a termination
      takeMVar readyToTerminate

      -- Terminate all and exit
      mapM_ terminateAndWaitForProcess phs
      mapM_ takeMVar logFinishes
      stopLogger logger
      takeMVar logFinished
      putStrLn "bye"
      return ExitSuccess

-- Run given app with given logger.
runApp :: Logger -> App -> IO (ProcessHandle, MVar ())
runApp logger App {name,cmd,args,envs} =  do
    -- Build environment variables to run app.
    -- Priority: 1. Procfile, 2. dotenv, 3. the environment
    envs' <- nub . mconcat <$> sequence [return envs, getEnvsInDotEnvFile,  getEnvironment]
    (master, _, ph) <- runInPseudoTerminal (proc cmd args) { env = Just envs' }
    m <- newEmptyMVar
    _ <- forkIO $ installLogger name logger master m
    return (ph,m)
  where
    getEnvsInDotEnvFile :: IO [Env]
    getEnvsInDotEnvFile = do
        d <- doesFileExist ".env"
        if d then Dotenv.parseFile ".env" else return []
