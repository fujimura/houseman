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
import           Test.QuickCheck      hiding (Failure, Result, Success)

genValue :: Gen String
genValue = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_/")

newtype RawProcfile = RawProcfile String deriving (Eq,Show,Ord)

genEnvKey :: Gen String
genEnvKey = listOf1 $ elements ['A'..'Z']

genProcName :: Gen String
genProcName = listOf1 $ elements (['A'..'Z'] ++ ['0'..'9'])

genRawProcfileLine :: Gen String
genRawProcfileLine = do
      n <- (++ ":") <$> genProcName
      c <- genValue
      a <- mconcat . intersperse " " <$> listOf genValue
      e <- mconcat <$> sequence [genEnvKey, return "=", genValue]
      e' <- mconcat <$> sequence [genEnvKey, return "=", genValue]
      return $ mconcat . intersperse " " $ [n, e, c, a, e']

instance Arbitrary RawProcfile where
  arbitrary = do
    n <- choose (1,10)
    RawProcfile . mconcat . intersperse "\n" <$> vectorOf n genRawProcfileLine

parse :: Parser a -> ByteString -> a
parse p bs = case parseByteString p mempty bs of
               Success x -> x
               Failure doc -> error (show doc)

shouldParse :: Show a => Parser a -> String -> Expectation
p `shouldParse` s = case parseString p mempty s of
                          Success _ -> return ()
                          Failure d -> error (show d)

shouldNotParse :: Show a => Parser a -> ByteString -> Expectation
p `shouldNotParse` bs = case parseByteString p mempty bs of
                          Success x -> expectationFailure ("Parsed to: " ++ show x)
                          Failure _ -> return ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Procfile.Parse" $ do
    describe "procfile" $ do
      it "should parse well" $ property $ \(RawProcfile r) ->
        Parse.procfile `shouldParse` r

      it "should parse web: foo\\nworker: bar" $
        parse Parse.procfile "web: foo\nworker: bar" `shouldBe`
          [ App "web" "foo" [] [], App "worker" "bar" [] []]

    describe "proc" $ do
      it "should parse web: some/application run" $
        parse Parse.proc "web: some/application run" `shouldBe` App { name = "web"
                                                                    , cmd = "some/application"
                                                                    , args = ["run"]
                                                                    , envs = []
                                                                    }
      it "should parse web: some/application run FOO=1" $
        parse Parse.proc "web: some/application run foo BAR=1" `shouldBe` App { name = "web"
                                                                              , cmd = "some/application"
                                                                              , args = ["run", "foo"]
                                                                              , envs = [("BAR", "1")]
                                                                              }
      it "should parse web: FOO=1 some/application run" $
        parse Parse.proc "web: FOO=1 some/application run" `shouldBe` App { name = "web"
                                                                          , cmd = "some/application"
                                                                          , args = ["run"]
                                                                          , envs = [("FOO", "1")]
                                                                          }
      it "should parse web: FOO=1 some/application run BAR=1" $
        parse Parse.proc "web: FOO=1 some/application run BAR=2" `shouldBe` App { name = "web"
                                                                                , cmd = "some/application"
                                                                                , args = ["run"]
                                                                                , envs = [("FOO", "1"), ("BAR", "2")]
                                                                                }

    describe "env" $ do
      it "should parse FOO=bar BAR=1" $
        parse Parse.env "FOO=bar BAR=1" `shouldBe` ("FOO", "bar")

      it "should parse FOO=bar baz" $
        parse Parse.env "FOO=bar baz" `shouldBe` ("FOO", "bar")

      it "should not parse FOO=" $
        Parse.env `shouldNotParse` "FOO="

      it "should not parse FOO" $
        Parse.env `shouldNotParse` "FOO"

      it "should not parse =bar" $
        Parse.env `shouldNotParse` "=bar"

      it "should not parse bar" $
        Parse.env `shouldNotParse` "bar"

      it "should parse FOO=\"double quoted\"" $
        parse Parse.env "FOO=\"double quoted\" BAR=1" `shouldBe` ("FOO", "double quoted")

      it "should parse FOO='single quoted'" $
        parse Parse.env "FOO='single quoted' BAR=1" `shouldBe` ("FOO", "single quoted")

      it "should parse FOO=\"\"" $
        parse Parse.env "FOO=\"\" BAR=1" `shouldBe` ("FOO", "")

      it "should parse FOO=\"es\\\"caped\"" $
        parse Parse.env "FOO=\"es\\\"caped\" BAR=1" `shouldBe` ("FOO", "es\"caped")
