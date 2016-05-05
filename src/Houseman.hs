{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman where

import           Control.Concurrent
import           Control.Monad
import           Data.Function
import           Data.List
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Posix.Signals
import           System.Process hiding (runProcess)

import qualified Configuration.Dotenv   as Dotenv
import           Data.Streaming.Process (StreamingProcessHandle)

import           Houseman.Internal      (bracketOnErrorMany,
                                         runProcess,
                                         terminateAndWaitForProcess,
                                         withAllExit, withAnyExit)
import           Houseman.Logger        (installLogger, newLogger, runLogger,
                                         stopLogger)
import           Houseman.Types
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
    bracketOnErrorMany (map (runApp logger) apps) terminateAndWaitForProcess $ \phs -> do
      -- Get a MVar to detect termination of a process
      readyToTerminate <- newEmptyMVar

      -- Output logs to stdout
      runLogger logger

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
      stopLogger logger
      putStrLn "bye"
      return ExitSuccess

-- Run given app with given logger.
runApp :: Logger -> App -> IO StreamingProcessHandle
runApp logger App {name,cmd} =  do
    -- Build environment variables to run app.
    -- .env supersedes environment from current process.
    envs <- nubBy ((==) `on` fst)  . mconcat <$> sequence [getEnvsInDotEnvFile, getEnvironment]
    (out,err,ph) <- runProcess (shell cmd) { env = Just envs }
    _ <- forkIO $ forM_ [out,err] (installLogger name logger)
    return ph
  where
    getEnvsInDotEnvFile :: IO [Env]
    getEnvsInDotEnvFile = do
        d <- doesFileExist ".env"
        if d then Dotenv.parseFile ".env" else return []
