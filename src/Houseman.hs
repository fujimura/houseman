{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Cont
import           Data.Function
import           Data.List
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Posix.Signals
import           System.Process

import qualified Configuration.Dotenv   as Dotenv
import           Data.Streaming.Process (StreamingProcessHandle)

import           Houseman.Internal      (terminateAndWaitForProcess,
                                         withAllExit, withAnyExit, withProcess)
import           Houseman.Logger        (installLogger, newLogger, runLogger,
                                         stopLogger)
import           Houseman.Types
import           Procfile.Types

-- | Runs `App` in `Procfile` with given name.
run :: String -> Procfile -> IO ExitCode
run cmd' apps = case find (\App{cmd} -> cmd == cmd') apps of
                  Just app -> Houseman.start [app] -- TODO Remove color in run command
                  Nothing   -> die ("Command '" ++ cmd' ++ "' not found in Procfile")

-- | Starts all `App`s in given `Procfile`.
start :: Procfile -> IO ExitCode
start apps = do
    print apps

    -- Allocate logger
    logger <- newLogger

    -- Run apps
    (`runContT` return) $ do
      phs <- mapM (ContT . withApp logger) apps
      liftIO $ do
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

        -- Wait for the termination
        takeMVar readyToTerminate

        -- Terminate all and exit
        mapM_ terminateAndWaitForProcess phs
        stopLogger logger
        putStrLn "bye"
        return ExitSuccess

-- | Runs given `App` with given `Logger`, invokes action with the process
-- handle of the `App`, and returns result of the action.
withApp :: Logger -> App -> (StreamingProcessHandle -> IO a) -> IO a
withApp logger App {name,cmd} action = do
    -- Build environment variables to run app.
    -- .env supersedes environment from current process.
    envs <- nubBy ((==) `on` fst)  . mconcat <$> sequence [getEnvsInDotEnvFile, getEnvironment]
    withProcess (shell cmd) { env = Just envs } $ \(out,err,ph) -> do
      _ <- forkIO $ forM_ [out,err] (installLogger name logger)
      action ph
  where
    getEnvsInDotEnvFile :: IO [Env]
    getEnvsInDotEnvFile = do
        d <- doesFileExist ".env"
        if d then Dotenv.parseFile ".env" else return []
