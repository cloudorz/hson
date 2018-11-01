module ParserSpec where

import Test.Hspec
import Parser
import Text.Megaparsec
import Data.Either

spec :: Spec
spec = do 
  describe "Parser JSON" $ do 
    describe "jString: parse string " $ do 
      it "parse \"abc cde\"" $ do
        parse jString "(undfined)" "\"abc cde\"" `shouldBe` return "abc cde"
      it "parse \"abc cde\"   " $ do
        parse jString "(undfined)" "\"abc cde\"   " `shouldBe` return "abc cde"
      it "can't parse \"abc cde\"abc   " $ do
        parse jString "(undfined)" "\"abc cde\"abc   " `shouldBe` return "abc cde"
      it "can't parse \"abc cde\",   " $ do
        parse jString "(undfined)" "\"abc cde\",   " `shouldBe` return "abc cde"
      it "parse \"abc cde\"   ,   " $ do
        parse jString "(undfined)" "\"abc cde\"  ,   " `shouldBe` return "abc cde"
      it "parse \"abc cde\"   :   " $ do
        parse jString "(undfined)" "\"abc cde\"  :   " `shouldBe` return "abc cde"
      it "parse \"abc cde\"   }   " $ do
        parse jString "(undfined)" "\"abc cde\"  }   " `shouldBe` return "abc cde"
      it "parse \"abc cde\"   ]   " $ do
        parse jString "(undfined)" "\"abc cde\"  ]   " `shouldBe` return "abc cde"
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
          parse jBool "(undfined)" "true," `shouldBe` return True
        it "parse true " $ do 
          parse jBool "(undfined)" "true " `shouldBe` return True
        it "can't parse True " $ do 
          parse jBool "(undfined)" "True " `shouldSatisfy` isLeft
      context "parse bool to False value" $ do
        it "parse false" $ do
          parse jBool "(undfined)" "false" `shouldBe` return False
        it "can't parse false," $ do
          parse jBool "(undfined)" "false," `shouldBe` return False
        it "parse false " $ do
          parse jBool "(undfined)" "false " `shouldBe` return False
        it "can't parse False " $ do
          parse jBool "(undfined)" "False " `shouldSatisfy` isLeft
    describe "jFloat: parse float" $ do
      it "parse 1.0 " $ do 
        parse jFloat "(undfined)" "1.0" `shouldBe` return 1.0
      it "parse 0.0" $ do 
        parse jFloat "(undfined)" "0.0" `shouldBe` return 0.0
      it "can't parse 0" $ do 
        parse jFloat "(undfined)" "0" `shouldSatisfy` isLeft
      it "can't parse 1.0, " $ do 
        parse jFloat "(undfined)" "1.0," `shouldBe` return 1.0
      it "can't parse 1.0 , " $ do 
        parse jFloat "(undfined)" "1.0 ," `shouldBe` return 1.0
      it "parse -1.0 " $ do 
        parse jFloat "(undfined)" "-1.0" `shouldBe` return (-1.0)
      it "parse -  1.0 " $ do 
        parse jFloat "(undfined)" "-  1.0" `shouldBe` return (-1.0)
      it "parse +1.0" $ do 
        parse jFloat "(undfined)" "+1.0" `shouldBe` return 1.0
      it "parse +  1.0 " $ do 
        parse jFloat "(undfined)" "+  1.0" `shouldBe` return 1.0
    describe "jInteger: parse integer" $ do
      it "parse 1" $ do
        parse jInteger "(undfined)" "1" `shouldBe` return 1
      it "parse 0" $ do 
        parse jInteger "(undfined)" "0" `shouldBe` return 0
      it "can't parse 1," $ do
        parse jInteger "(undfined)" "1," `shouldBe` return 1
      it "can't parse 1 ," $ do
        parse jInteger "(undfined)" "1 ," `shouldBe` return 1
      it "parse -1" $ do
        parse jInteger "(undfined)" "-1" `shouldBe` return (-1)
      it "parse -   1" $ do
        parse jInteger "(undfined)" "-  1" `shouldBe` return (-1)
      it "parse +1" $ do
        parse jInteger "(undfined)" "+1" `shouldBe` return 1
    describe "jArray: parse json array" $ do
      it "parse [1, 2, 3]" $ do
        parse jArray "(undfined)" "[1, 2, 3]" `shouldBe` return [N 1, N 2, N 3]
      it "parse [\"a\", \"b\", \"c\"]" $ do
        parse jArray "(undfined)" "[\"a\", \"b\", \"c\"]" `shouldBe` return [S "a", S "b", S "c"]
      it "parse [\"a\", 1, 0.1]" $ do
        parse jArray "(undfined)" "[\"a\", 1, 0.1]" `shouldBe` return [S "a", N 1, F 0.1]
      it "pase [{\"a\": 1}, {\"a\": \"c\"}]" $ do
        parse jArray "(undfined)" "[{\"a\": 1}, {\"a\": \"c\"}]" `shouldBe` return [Object [("a", N 1)], Object [("a", S "c")]]
      it "parse [[1, 2], [\"a\", 2.2]]" $ do 
        parse jArray "(undfined)" "[[1, 2], [\"a\", 2.2]]" `shouldBe` return [Array [N 1, N 2], Array [S "a", F 2.2]]
    describe "jObject: parse json object" $ do
      it "parse {\"abc\" : \"abc\"}" $ do
        parse jObject "(undfined)" "{\"abc\" : \"abc\"}" `shouldBe` return [("abc", S "abc")]
      it "parse {\"abc\" : \"abc\", \"n2\" : 233, \"n3\": 2.33}" $ do
        parse jObject "(undfined)" "{\"abc\" : \"abc\", \"n2\" : 233, \"n3\": 2.33}" `shouldBe` return [("abc", S "abc"), ("n2", N 233), ("n3", F 2.33)]
      it "parse {\"array\": [1, \"a\", 1.1]}" $ do
        parse jObject "(undfined)" "{\"array\": [1, \"a\", 1.1]}" `shouldBe` return [("array", Array [N 1, S "a", F 1.1])]
      it "parse {\"abc\": {\"abc\": 2.22}}" $ do
        parse jObject "(undfined)" "{\"abc\": {\"abc\": 2.22}}" `shouldBe` return [("abc", Object [("abc", F 2.22)])]
    describe "jDecode: parse json" $ do
      it "pase {\"a\": [1, 2, 3], \"b\": \"vvv\", \"c\": {\"abc\": \"string\"}}" $ do
        parse jDecode "(undfined)" "  {\"a\": [1, 2, 3], \"b\": \"vvv\", \"c\": {\"abc\": \"string\"}}" `shouldBe` return (Object [("a", Array [N 1, N 2, N 3]), ("b", S "vvv"), ("c", Object [("abc", S "string")])])
