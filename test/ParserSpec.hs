module ParserSpec where

import Test.Hspec
import Parser
import Text.Megaparsec

spec :: Spec
spec = do 
  describe "Parser JSON" $ do 
    describe "jString: parse string " $ do 
      it "parse \"abc cde\"" $ do
        parse jString "(undfined)" "\"abc cde\"" == return "abc cde"
      it "parse \"abc cde\"   " $ do
        parse jString "(undfined)" "\"abc cde\"   " == return "abc cde"
      it "can't parse \"abc cde\"abc   " $ do
        parse jString "(undfined)" "\"abc cde\"abc   " /= return "abc cde"
      it "can't parse \"abc cde\",   " $ do
        parse jString "(undfined)" "\"abc cde\",   " /= return "abc cde"
      it "parse \r" $ do
        parse jString "(undfined)" "\"\r\" " == return "\r"
      it "parse \t" $ do
        parse jString "(undfined)" "\"\t\" " == return "\t"
      it "parse \n" $ do
        parse jString "(undfined)" "\"\n\" " == return "\n"
    describe "jBool: parse bool" $ do
      context "parse bool to True value" $ do
        it "parse true" $ do 
          parse jBool "(undfined)" "true" == return True
        it "can't parse true," $ do 
          parse jBool "(undfined)" "true," /= return True
        it "can't parse trueabc" $ do 
          parse jBool "(undfined)" "trueabc" /= return True
        it "parse true " $ do 
          parse jBool "(undfined)" "true " == return True
        it "can't parse True " $ do 
          parse jBool "(undfined)" "True " /= return True
      context "parse bool to False value" $ do
        it "parse false" $ do
          parse jBool "(undfined)" "false" == return False
        it "can't parse false," $ do
          parse jBool "(undfined)" "false," /= return False
        it "can't parse falseabc" $ do
          parse jBool "(undfined)" "falseabc" /= return False
        it "parse false " $ do
          parse jBool "(undfined)" "false " == return False
        it "can't parse False " $ do
          parse jBool "(undfined)" "False " /= return False
    describe "jFloat: parse float" $ do
      it "parse 1.0 " $ do 
        parse jFloat "(undfined)" "1.0" == return 1.0
      it "parse 0.0" $ do 
        parse jFloat "(undfined)" "0.0" == return 0.0
      it "can't parse 0" $ do 
        parse jFloat "(undfined)" "0" /= return 0
      it "can't parse 1.0, " $ do 
        parse jFloat "(undfined)" "1.0," /= return 1.0
      it "can't parse 1.0 , " $ do 
        parse jFloat "(undfined)" "1.0 ," /= return 1.0
      it "parse -1.0 " $ do 
        parse jFloat "(undfined)" "-1.0" == return (-1.0)
      it "parse -  1.0 " $ do 
        parse jFloat "(undfined)" "-  1.0" == return (-1.0)
      it "parse +1.0" $ do 
        parse jFloat "(undfined)" "+1.0" == return 1.0
      it "parse +  1.0 " $ do 
        parse jFloat "(undfined)" "+  1.0" == return 1.0
      it "can't parse 11.0am" $ do 
        parse jFloat "(undfined)" "11.0am" /= return 11.0
    describe "jInteger: parse integer" $ do
      it "parse 1" $ do
        parse jInteger "(undfined)" "1" == return 1
      it "parse 0" $ do 
        parse jInteger "(undfined)" "0" == return 0
      it "can't parse 1," $ do
        parse jInteger "(undfined)" "1," /= return 1
      it "can't parse 1 ," $ do
        parse jInteger "(undfined)" "1 ," /= return 1
      it "can't parse 1.0 ," $ do
        parse jInteger "(undfined)" "1.0 ," /= return 1
      it "parse -1" $ do
        parse jInteger "(undfined)" "-1" == return (-1)
      it "parse -   1" $ do
        parse jInteger "(undfined)" "-  1" == return (-1)
      it "parse +1" $ do
        parse jInteger "(undfined)" "+1" == return 1
