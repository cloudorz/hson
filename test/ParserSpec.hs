module ParserSpec where

import Test.Hspec
import Parser
import Text.Megaparsec

spec :: Spec
spec = do 
  describe "Parser JSON" $ do 
    describe "jString: parse string " $ do 
      it "parse \"abc cde\"" $ do
        parse jString "(undfined)" "\"abc cde\"" `shouldBe` return "abc cde"
      it "parse \"abc cde\"   " $ do
        parse jString "(undfined)" "\"abc cde\"   " `shouldBe` return "abc cde"
      it "can't parse \"abc cde\"abc   " $ do
        parse jString "(undfined)" "\"abc cde\"abc   " `shouldNotBe` return "abc cde"
      it "can't parse \"abc cde\",   " $ do
        parse jString "(undfined)" "\"abc cde\",   " `shouldNotBe` return "abc cde"
      it "parse \r" $ do
        parse jString "(undfined)" "\"\r\" " `shouldBe` return "\r"
      it "parse \t" $ do
        parse jString "(undfined)" "\"\t\" " `shouldBe` return "\t"
      it "parse \n" $ do
        parse jString "(undfined)" "\"\n\" " `shouldBe` return "\n"
    describe "jBool: parse bool" $ do
      context "parse bool to True value" $ do
        it "parse true" $ do 
          parse jBool "(undfined)" "true" `shouldBe` return True
        it "can't parse true," $ do 
          parse jBool "(undfined)" "true," `shouldNotBe` return True
        it "can't parse trueabc" $ do 
          parse jBool "(undfined)" "trueabc" `shouldNotBe` return True
        it "parse true " $ do 
          parse jBool "(undfined)" "true " `shouldBe` return True
        it "can't parse True " $ do 
          parse jBool "(undfined)" "True " `shouldNotBe` return True
      context "parse bool to False value" $ do
        it "parse false" $ do
          parse jBool "(undfined)" "false" `shouldBe` return False
        it "can't parse false," $ do
          parse jBool "(undfined)" "false," `shouldNotBe` return False
        it "can't parse falseabc" $ do
          parse jBool "(undfined)" "falseabc" `shouldNotBe` return False
        it "parse false " $ do
          parse jBool "(undfined)" "false " `shouldBe` return False
        it "can't parse False " $ do
          parse jBool "(undfined)" "False " `shouldNotBe` return False
    describe "jFloat: parse float" $ do
      it "parse 1.0 " $ do 
        parse jFloat "(undfined)" "1.0" `shouldBe` return 1.0
      it "parse 0.0" $ do 
        parse jFloat "(undfined)" "0.0" `shouldBe` return 0.0
      it "can't parse 0" $ do 
        parse jFloat "(undfined)" "0" `shouldNotBe` return 0
      it "can't parse 1.0, " $ do 
        parse jFloat "(undfined)" "1.0," `shouldNotBe` return 1.0
      it "can't parse 1.0 , " $ do 
        parse jFloat "(undfined)" "1.0 ," `shouldNotBe` return 1.0
      it "parse -1.0 " $ do 
        parse jFloat "(undfined)" "-1.0" `shouldBe` return (-1.0)
      it "parse -  1.0 " $ do 
        parse jFloat "(undfined)" "-  1.0" `shouldBe` return (-1.0)
      it "parse +1.0" $ do 
        parse jFloat "(undfined)" "+1.0" `shouldBe` return 1.0
      it "parse +  1.0 " $ do 
        parse jFloat "(undfined)" "+  1.0" `shouldBe` return 1.0
      it "can't parse 11.0am" $ do 
        parse jFloat "(undfined)" "11.0am" `shouldNotBe` return 11.0
    describe "jInteger: parse integer" $ do
      it "parse 1" $ do
        parse jInteger "(undfined)" "1" `shouldBe` return 1
      it "parse 0" $ do 
        parse jInteger "(undfined)" "0" `shouldBe` return 0
      it "can't parse 1," $ do
        parse jInteger "(undfined)" "1," `shouldNotBe` return 1
      it "can't parse 1 ," $ do
        parse jInteger "(undfined)" "1 ," `shouldNotBe` return 1
      it "can't parse 1.0 ," $ do
        parse jInteger "(undfined)" "1.0 ," `shouldNotBe` return 1
      it "parse -1" $ do
        parse jInteger "(undfined)" "-1" `shouldBe` return (-1)
      it "parse -   1" $ do
        parse jInteger "(undfined)" "-  1" `shouldBe` return (-1)
      it "parse +1" $ do
        parse jInteger "(undfined)" "+1" `shouldBe` return 1
