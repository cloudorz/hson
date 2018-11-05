module HsonSpec where

import Test.Hspec
import Hson

spec :: Spec
spec = do 
  describe "Hson" $ do 
    it "decode: parse JSON string to structure string" $ do
      decode  "  {\"a\": [1, 2, 3], \"b\": \"vvv\", \"c\": {\"abc\": \"string\"}}" `shouldBe` "Object [(\"a\",Array [N 1,N 2,N 3]),(\"b\",S \"vvv\"),(\"c\",Object [(\"abc\",S \"string\")])]"
