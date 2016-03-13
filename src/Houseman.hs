{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman where

import           Control.Concurrent
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Data.Text             (Text)
import           Data.Time
import           GHC.IO.Handle
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Signals
import           System.Posix.Types
import           System.Process

import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified System.Posix.IO       as IO
import qualified System.Posix.Terminal as Terminal

import           Procfile.Types

run :: String -> Procfile -> IO ()
run cmd' procs = case find (\Proc{cmd} -> cmd == cmd') procs of
                  Just proc -> Houseman.start [proc] -- TODO Remove color in run command
                  Nothing   -> die ("Command '" ++ cmd' ++ "' not found in Procfile")

colors :: [Color]
colors = cycle [32..36]

colorString :: Color -> Text -> Text
colorString c x = "\x1b[" <> Text.pack (show c) <> "m" <> x <> "\x1b[0m"

start :: [Proc] -> IO ()
start procs = do
    print procs
    phs <- zipWithM Houseman.runProcess procs colors
    m <- newEmptyMVar
    installHandler keyboardSignal (Catch (handler m phs)) Nothing
    forkIO $ forever $ do
      threadDelay 1000
      b <- any failed <$> mapM getProcessExitCode phs
      when b $ do
        -- TODO Maybe we don't have to try to terminate process which already
        -- returned exit code.
        -- TODO I'm not totally sure SIGTERM is best here, but there seems
        -- to be no way to send signal other than it.
        forM_ phs terminateAndWaitForProcess
        putMVar m (ExitFailure 1)
    exitStatus <- takeMVar m
    putStrLn "bye"
    exitWith exitStatus
  where
    handler :: MVar ExitCode -> [ProcessHandle] -> IO ()
    handler m phs = do
      forM_ phs terminateAndWaitForProcess
      void $ putMVar m (ExitFailure 1)
    failed :: Maybe ExitCode -> Bool
    failed (Just (ExitFailure _)) = True
    failed _                      = False
    terminateAndWaitForProcess :: ProcessHandle -> IO ()
    terminateAndWaitForProcess ph = terminateProcess ph >> waitForProcess ph >> return ()

runProcess :: Proc -> Color -> IO ProcessHandle
runProcess Proc {name,cmd,args,envs} color =  do
    currentEnvs <- getEnvironment
    (master, _, ph) <- runInPseudoTerminal (proc cmd args) { env = Just (envs ++ currentEnvs) }
    forkIO $ readLoop master
    return ph
  where
    readLoop read = do
      c <- (||) <$> hIsClosed read <*> hIsEOF read
      unless c $ do
        Text.hGetLine read >>= \l -> do
          t <- Text.pack <$> formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
          Text.putStrLn (colorString color (t <> " " <> Text.pack name <> ": ") <> l )
        threadDelay 1000
        readLoop read

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
