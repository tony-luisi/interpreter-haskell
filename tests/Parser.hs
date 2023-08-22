import Parser
import Lexer
import Interpreter
import Test.Hspec


main = hspec $ do
    describe "parse" $ do
        it "should parse an integer" $ do
            parse [IntToken 1] `shouldBe` IntValue 1
        it "should parse an ending semicolon" $ do
            parse [IntToken 1, SemicolonToken] `shouldBe` Sequence [IntValue 1]
            parse [IntToken 1, SemicolonToken, IntToken 1] `shouldBe` Sequence [IntValue 1, IntValue 1]
    describe "parseFactor" $ do
        it "should parse an integer" $ do
            parseFactor [IntToken 1] `shouldBe` (IntValue 1, [])
        it "should parse an integer" $ do
            parseFactor [IntToken 1, OperatorToken "+", IntToken 1] `shouldBe` (IntValue 1, [OperatorToken "+", IntToken 1])
        it "should parse parentheses" $ do
            parseFactor [OpenParenthesisToken, IntToken 1, CloseParenthesisToken] `shouldBe` (IntValue 1, [])
    describe "parseTerm" $ do
        it "should parse an integer" $ do
            parseTerm [IntToken 1] `shouldBe` (IntValue 1, [])
    describe "parseExpression" $ do
        it "should parse an integer" $ do
            parseExpression [IntToken 1] `shouldBe` (IntValue 1, [])
        it "should parse addition" $ do
            parseExpression [IntToken 1, OperatorToken "+", IntToken 1] `shouldBe` (Operator "+" (IntValue 1) (IntValue 1), [])
        it "should parse multiplication" $ do
            parseExpression [IntToken 2, OperatorToken "*", IntToken 2] `shouldBe` (Operator "*" (IntValue 2) (IntValue 2), [])
        it "should parse addition and multiplication" $ do
            parseExpression [IntToken 2, OperatorToken "*", IntToken 2, OperatorToken "+", IntToken 2] `shouldBe` (Operator "+" (Operator "*" (IntValue 2) (IntValue 2)) (IntValue 2), [])
        it "should parse addition, multiplication and parentheses"  $ do
            parseExpression [IntToken 2, OperatorToken "*", OpenParenthesisToken, IntToken 2, OperatorToken "+", IntToken 2, CloseParenthesisToken] `shouldBe` (Operator "*" (IntValue 2) (Operator "+" (IntValue 2) (IntValue 2)), [])
