{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HousemanSpec ( main, spec ) where

import           Control.Concurrent
import           System.Environment
import           System.Process

import           Data.Streaming.Process (waitForStreamingProcess)

import qualified Houseman
import           Houseman.Logger        (newLogger, readLogger)
import           Houseman.Types
import           Procfile.Types

import           System.IO.Silently     (capture)
import           Test.Hspec
import           Test.Mockery.Directory (inTempDirectory)


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Houseman" $ do
  describe "start" $ do
    it "should run given apps" $ do
      let apps = [ App "echo1" "./test/fixtures/echo.sh" ["foo", "bar"] []
                 , App "echo2" "./test/fixtures/echo.sh" ["baz", "🙈"] []
                 ]
      (result,_) <- capture $ Houseman.start apps
      result `shouldContain` "echo1: \ESC[0mfoo"
      result `shouldContain` "echo1: \ESC[0mbar"
      result `shouldContain` "echo2: \ESC[0mbaz"
      result `shouldContain` "echo2: \ESC[0m🙈"

  describe "runApp" $ do
    it "should run given process" $ do
      log' <- newLogger
      _ <- capture . waitForStreamingProcess . fst =<< Houseman.runApp log' (App "echo" "./test/fixtures/echo.sh" ["foo", "🙈"] [("ECHO", "1")])
      readLogger log' `shouldReturn` Log ("echo", "ECHO=1")
      readLogger log' `shouldReturn` Log ("echo", "foo")
      readLogger log' `shouldReturn` Log ("echo", "🙈")

    it "should use .env as dotenv file, which supersedes original environment" $ inTempDirectory $ do
      setEnv "BAZ" "3"
      writeFile ".env" "BAZ=2"
      log' <- newLogger
      _ <- capture . waitForStreamingProcess . fst =<< Houseman.runApp log' (App "echo" "printenv" ["BAZ"] [])
      readLogger log' `shouldReturn` Log ("echo", "2")
