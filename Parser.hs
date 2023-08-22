module Parser where

import Data.Functor.Contravariant (Op)
import Data.Sequence (Seq (Empty))
import Interpreter
import Lexer

-- | Parse a list of tokens into an expression.
parse :: [Token] -> Expression
parse tokens =
  let (exp, rest) = parseExpression tokens
   in case rest of
        (SemicolonToken : rest1) -> filterEmptyExpression (Sequence [exp, parse rest1])
        [] -> exp
        _ -> error ("Invalid expression" ++ show rest)

-- | Filter out empty expressions.
filterEmptyExpression :: Expression -> Expression
filterEmptyExpression (Sequence x) = Sequence (filter (/= EmptyExpression) x)

-- parse an expression
parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens =
  let (term, rest) = parseTerm tokens
   in case rest of
        (OperatorToken op : rest1)
          | op `elem` ["+", "-"] ->
              let (expression, rest2) = parseExpression rest1
               in (Operator op term expression, rest2)
        _ -> (term, rest)

-- parse a term
parseTerm :: [Token] -> (Expression, [Token])
parseTerm tokens = do
  let (factor, rest) = parseFactor tokens
  case rest of
    (OperatorToken op : rest1)
      | op `elem` ["*", "/"] ->
          let (term, rest2) = parseTerm rest1
           in (Operator op factor term, rest2)
    _ -> (factor, rest)

-- parse a factor
parseFactor :: [Token] -> (Expression, [Token])
parseFactor (IntToken x : xs) = (IntValue x, xs)
parseFactor [] = (EmptyExpression, [])
parseFactor (OpenParenthesisToken : xs) =
  let (expression, rest) = parseExpression xs
   in case rest of
        (CloseParenthesisToken : rest1) -> (expression, rest1)
        _ -> error "Invalid expression"
parseFactor x = (EmptyExpression, x)
