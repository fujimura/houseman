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
      (result,_) <- capture $ do
        ph <- Houseman.runProcess (Proc "echo" "./test/fixtures/echo.sh" ["foo", "🙈"] [("ECHO", "1")]) 31
        -- TODO This may not work in super fast machine
        threadDelay (1000 * 1000)
        waitForProcess ph
      let [one,two,three] = lines result
      one `shouldEndWith` "echo: \ESC[0mECHO=1" -- TODO UTF8
      two `shouldEndWith` "echo: \ESC[0mfoo"
      three `shouldEndWith` "echo: \ESC[0m🙈"
