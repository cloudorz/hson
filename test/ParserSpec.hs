module ParserSpec where

import Test.Hspec
import Parser
import Text.Megaparsec

spec :: Spec
spec = do 
  describe "Parser JSON" $ do 
    describe "parse string" $ do 
      context "given \"abc cde\", zzz" $ do 
        it "can parse \"abc cde\"" $ do
          parse jsonString "(undfined)" "\"abc cde\"zzz" `shouldBe` return "abc cde"
