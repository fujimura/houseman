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
                                       watchFailureOfProcesses,
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
      m <- newEmptyMVar

      -- Output logs to stdout
      m' <- runLogger logger

      -- Fill MVar with signal
      [sigINT, sigTERM, keyboardSignal] `forM_` \signal ->
        installHandler signal (Catch (putMVar m ())) Nothing

      -- Fill MVar with any process termination
      _ <- forkIO $ watchFailureOfProcesses (putMVar m ()) phs
      _ <- forkIO $ withAllExit (== ExitSuccess) phs (putMVar m ())

      -- Wait a termination
      takeMVar m

      -- Terminate all and exit
      forM_ phs terminateAndWaitForProcess
      mapM_ takeMVar logFinishes
      stopLogger logger
      takeMVar m'
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
