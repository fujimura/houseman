{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Procfile.ParseSpec ( main, spec ) where

import           Data.ByteString      (ByteString)
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Data.List
import qualified Procfile.Parse       as Parse
import           Procfile.Types

import           Test.Hspec

parse :: Parser a -> ByteString -> a
parse p bs = case parseByteString p mempty bs of
               Success x -> x
               Failure doc -> error (show doc)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Procfile.Parse" $ do
    describe "procfile" $ do
      it "should parse web: foo\nworker: bar" $
        parse Parse.procfile "web: foo -a 1\nworker: bar --b=c baz" `shouldBe`
          [ App "web" "foo -a 1", App "worker" "bar --b=c baz"]
