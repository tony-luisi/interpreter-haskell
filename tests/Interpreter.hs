import Interpreter
import Test.Hspec

main = hspec $ do
  describe "test1" $ do
    it "should be 1" $ do
      1 `shouldBe` 1
  describe "evaluate" $ do
    it "should evaluate IntValue" $ do
      evaluate (IntValue 1) `shouldBe` IntValue 1
      evaluate (IntValue 5) `shouldBe` IntValue 5
    it "should evaluate a Sequence" $ do
      evaluate (Sequence []) `shouldBe` IntValue 0
      evaluate (Sequence [IntValue 1]) `shouldBe` IntValue 1
      evaluate (Sequence [IntValue 1, IntValue 2]) `shouldBe` IntValue 2
      evaluate (Sequence [IntValue 3, IntValue 4]) `shouldBe` IntValue 4
