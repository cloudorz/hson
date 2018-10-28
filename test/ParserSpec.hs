module ParserSpec where

import Test.Hspec
import Parser
import Text.Megaparsec

spec :: Spec
spec = do 
  describe "Parser JSON" $ do 
    describe "parse string" $ do 
      it "parse \"abc cde\", zzz" $ do
        parse jString "(undfined)" "\"abc cde\", zzz" `shouldBe` return "abc cde"
      it "parse \r, zzz" $ do
        parse jString "(undfined)" "\"\r\", zzz" `shouldBe` return "\r"
      it "parse \t, zzz" $ do
        parse jString "(undfined)" "\"\t\", zzz" `shouldBe` return "\t"
      it "parse \n, zzz" $ do
        parse jString "(undfined)" "\"\n\", zzz" `shouldBe` return "\n"
    describe "parse bool" $ do
      context "parse bool to True value" $ do
        it "parse true" $ do 
          parse jBool "(undfined)" "true" == return True
        it "parse true," $ do 
          parse jBool "(undfined)" "true," == return True
        it "can't parse trueabc" $ do 
          parse jBool "(undfined)" "trueabc" /= return True
        it "parse true " $ do 
          parse jBool "(undfined)" "true " == return True
        it "can't parse True " $ do 
          parse jBool "(undfined)" "True " /= return True
      context "parse bool to False value" $ do
        it "parse false" $ do
          parse jBool "(undfined)" "false" == return False
        it "parse false," $ do
          parse jBool "(undfined)" "false," == return False
        it "can't parse falseabc" $ do
          parse jBool "(undfined)" "falseabc" /= return False
        it "parse false " $ do
          parse jBool "(undfined)" "false " == return False
        it "can't parse False " $ do
          parse jBool "(undfined)" "False " /= return False
