{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HousemanSpec ( main, spec ) where

import           Control.Concurrent
import           System.Environment
import           System.Process

import qualified Houseman
import           Procfile.Types

import           System.IO.Silently     (capture)
import           Test.Hspec
import           Test.Mockery.Directory (inTempDirectory)


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Houseman" $ do
  describe "runApp" $ do
    it "should run given process" $ do
      log' <- newChan
      _ <- capture . waitForProcess =<< Houseman.runApp (App "echo" "./test/fixtures/echo.sh" ["foo", "🙈"] [("ECHO", "1")]) log'
      readChan log' `shouldReturn` ("echo", "ECHO=1")
      readChan log' `shouldReturn` ("echo", "foo")
      readChan log' `shouldReturn` ("echo", "🙈")

    it "should use .env as dotenv file, which supersedes original environment" $ inTempDirectory $ do
      setEnv "BAZ" "3"
      writeFile ".env" "BAZ=2"
      log' <- newChan
      _ <- capture . waitForProcess =<< Houseman.runApp (App "echo" "printenv" ["BAZ"] []) log'
      readChan log' `shouldReturn` ("echo", "2")
