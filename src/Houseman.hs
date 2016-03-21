{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Houseman where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import           Data.Time
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix.Signals
import           System.Posix.Types
import           System.Process

import qualified Configuration.Dotenv    as Dotenv
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified System.Posix.IO         as IO
import qualified System.Posix.Terminal   as Terminal

import           Procfile.Types

type Log = (String, Text)

run :: String -> Procfile -> IO ()
run cmd' apps = case find (\App{cmd} -> cmd == cmd') apps of
                  Just proc -> Houseman.start [proc] -- TODO Remove color in run command
                  Nothing   -> die ("Command '" ++ cmd' ++ "' not found in Procfile")

colors :: [Color]
colors = cycle [32..36]

colorString :: Color -> Text -> Text
colorString c x = "\x1b[" <> Text.pack (show c) <> "m" <> x <> "\x1b[0m"

start :: [App] -> IO ()
start apps = do
    print apps
    log <- newChan
    withProcesses log apps $ \phs -> do
      m <- newEmptyMVar

      installHandler keyboardSignal (Catch (terminateAll m phs)) Nothing

      _ <- forkIO $ waitForProcessesAndTerminateAll m phs
      _ <- forkIO $ outputLog log

      exitStatus <- takeMVar m
      putStrLn "bye"
      exitWith exitStatus
  where
    withProcesses :: Chan Log -> [App] -> ([ProcessHandle] -> IO ()) -> IO ()
    withProcesses log apps action = do
        phs <- foldM go [] apps
        action phs
      where
        go :: [ProcessHandle] -> App -> IO [ProcessHandle]
        go phs p = bracket (runApp p log)
                           terminateAndWaitForProcess
                           (\ph -> return (phs ++ [ph]))

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

    outputLog :: Chan Log -> IO ()
    outputLog log = go []
      where
        go cs = do
          (name,l) <- readChan log
          let (color, cs') = name `lookupOrInsertNewColor` cs
          t <- Text.pack <$> formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
          Text.putStrLn (colorString color (t <> " " <> Text.pack name <> ": ") <> l )
          threadDelay 1000
          go cs'
        lookupOrInsertNewColor :: String -> [(String, Color)] -> (Color, [(String, Color)])
        lookupOrInsertNewColor x xs = case x `lookup` xs of
                                          Just color -> (color,xs)
                                          Nothing -> let color = colors !! length xs
                                                         in (color,(x,color):xs)

runApp :: App -> Chan Log -> IO ProcessHandle
runApp App {name,cmd,args,envs} log =  do
    currentEnvs <- getEnvironment
    d <- doesFileExist ".env"
    dotEnvs <- if d then Dotenv.parseFile ".env" else return []
    (master, _, ph) <- runInPseudoTerminal (proc cmd args) { env = Just (nub $ envs ++ dotEnvs ++ currentEnvs)}
    forkIO $ readLoop master
    return ph
  where
    readLoop read = do
      c <- (||) <$> hIsClosed read <*> hIsEOF' read
      unless c $ do
        Text.hGetLine read >>= \l -> writeChan log (name,l)
        threadDelay 1000
        readLoop read
    hIsEOF' h = hIsEOF h `catchIOError` (\e -> if ioeGetErrorType e == HardwareFault then return True else ioError e)

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
