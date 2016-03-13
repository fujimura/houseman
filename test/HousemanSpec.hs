{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HousemanSpec ( main, spec ) where

import           Control.Concurrent
import           System.Process

import qualified Houseman
import           Procfile.Types

import           System.IO.Silently  (capture)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Houseman" $ do
  describe "runProcess" $ do
    it "should run given process" $ do
      log' <- newChan
      _ <- capture $ do
        ph <- Houseman.runProcess (Proc "echo" "./test/fixtures/echo.sh" ["foo", "🙈"] [("ECHO", "1")]) log'
        -- TODO This may not work in super fast machine
        threadDelay (1000 * 1000)
        waitForProcess ph
      readChan log' `shouldReturn` ("echo", "ECHO=1")
      readChan log' `shouldReturn` ("echo", "foo")
      readChan log' `shouldReturn` ("echo", "🙈")
