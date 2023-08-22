import Lexer 
import Test.Hspec

main = hspec $ do
    describe "test1" $ do
        it "should be 1" $ do
            1 `shouldBe` 1
    describe "tokenize" $ do
        it "should tokenize IntValue" $ do
            tokenize "1" `shouldBe` [IntToken 1]
            tokenize "5" `shouldBe` [IntToken 5]
        it "should tokenize expression addition expressions" $ do
            tokenize "1+1" `shouldBe` [IntToken 1, OperatorToken "+", IntToken 1]
            tokenize "1 + 1" `shouldBe` [IntToken 1, OperatorToken "+", IntToken 1]
            tokenize "10+11" `shouldBe` [IntToken 10, OperatorToken "+", IntToken 11]
        it "should tokenize BoolValue" $ do
            tokenize "true" `shouldBe` [BoolToken True]
            tokenize "false" `shouldBe` [BoolToken False]
        it "should tokenize StringValue" $ do
            tokenize "\"hello\"" `shouldBe` [StringToken "hello"]
        it "should tokenize ListValue" $ do
            tokenize "[]" `shouldBe` [OpenBracketToken, CloseBracketToken]
            tokenize "[1]" `shouldBe` [OpenBracketToken, IntToken 1, CloseBracketToken]
            tokenize "[1,2]" `shouldBe` [OpenBracketToken, IntToken 1, CommaToken, IntToken 2, CloseBracketToken]
            tokenize "[true, false]" `shouldBe` [OpenBracketToken, BoolToken True, CommaToken, BoolToken False, CloseBracketToken]
        it "should tokenize Sequence" $ do
            tokenize "1;2" `shouldBe` [IntToken 1, SemicolonToken, IntToken 2]
            tokenize "1;2;3" `shouldBe` [IntToken 1, SemicolonToken, IntToken 2, SemicolonToken, IntToken 3]
        it "should tokenize IdentifierValue" $ do
            tokenize "aname" `shouldBe` [IdentifierToken "aname"]
        it "should tokenize assignment expressions" $ do
            tokenize "x = 1" `shouldBe` [IdentifierToken "x", AssignmentToken, IntToken 1]
            tokenize "x=1" `shouldBe` [IdentifierToken "x", AssignmentToken, IntToken 1]
            tokenize "x = 1 + 2" `shouldBe` [IdentifierToken "x", AssignmentToken, IntToken 1, OperatorToken "+", IntToken 2]
        it "should tokenize if expressions" $ do
            tokenize "if (true) { 1 }" `shouldBe` [IfToken, OpenParenthesisToken, BoolToken True, CloseParenthesisToken, OpenBraceToken, IntToken 1, CloseBraceToken]
            tokenize "if (true) { 1 } else { 2 }" `shouldBe` [IfToken, OpenParenthesisToken, BoolToken True, CloseParenthesisToken, OpenBraceToken, IntToken 1, CloseBraceToken, ElseToken, OpenBraceToken, IntToken 2, CloseBraceToken]


        