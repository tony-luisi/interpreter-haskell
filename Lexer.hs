module Lexer(tokenize, Token(
    IntToken,
    BoolToken,
    StringToken,
    ListToken,
    OperatorToken,
    SequenceToken,
    IdentifierToken,
    AssignmentToken,
    IfToken,
    ElseToken,
    WhileToken,
    ForToken,
    InToken,
    DefToken,
    ReturnToken,
    PrintToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    OpenBraceToken,
    CloseBraceToken,
    OpenBracketToken,
    CloseBracketToken,
    CommaToken,
    ColonToken,
    SemicolonToken,
    EofToken

)) where

import Data.Char
import Data.List
import Data.Maybe
import System.IO

data Token
  = IntToken Int
  | BoolToken Bool
  | StringToken String
  | ListToken [Token]
  | OperatorToken String
  | SequenceToken [Token]
  | IdentifierToken String
  | AssignmentToken
  | IfToken
  | ElseToken
  | WhileToken
  | ForToken
  | InToken
  | DefToken
  | ReturnToken
  | PrintToken
  | OpenParenthesisToken
  | CloseParenthesisToken
  | OpenBraceToken
  | CloseBraceToken
  | OpenBracketToken
  | CloseBracketToken
  | CommaToken
  | ColonToken
  | SemicolonToken
  | EofToken
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
  | isSpace x = tokenize xs
  | isDigit x = tokenizeNumber (x : xs)
  | isAlpha x = tokenizeIdentifier (x : xs)
  | x == '"' = tokenizeString xs
  | x == '+' || x == '-' || x == '*' || x == '/' = tokenizeOperator (x : xs)
  | x == '(' = OpenParenthesisToken : tokenize xs
  | x == ')' = CloseParenthesisToken : tokenize xs
  | x == '{' = OpenBraceToken : tokenize xs
  | x == '}' = CloseBraceToken : tokenize xs
  | x == '[' = OpenBracketToken : tokenize xs
  | x == ']' = CloseBracketToken : tokenize xs
  | x == ',' = CommaToken : tokenize xs
  | x == ':' = ColonToken : tokenize xs
  | x == ';' = SemicolonToken : tokenize xs
  | x == '=' = AssignmentToken : tokenize xs
  | otherwise = error ("Cannot tokenize " ++ [x])

tokenizeNumber :: String -> [Token]
tokenizeNumber xs =
  let (number, rest) = span isDigit xs
   in IntToken (read number) : tokenize rest

tokenizeIdentifier :: String -> [Token]
tokenizeIdentifier xs =
  let (identifier, rest) = span isAlpha xs
   in case identifier of
        "if" -> IfToken : tokenize rest
        "else" -> ElseToken : tokenize rest
        "while" -> WhileToken : tokenize rest
        "for" -> ForToken : tokenize rest
        "in" -> InToken : tokenize rest
        "def" -> DefToken : tokenize rest
        "return" -> ReturnToken : tokenize rest
        "print" -> PrintToken : tokenize rest
        "true" -> BoolToken True : tokenize rest
        "false" -> BoolToken False : tokenize rest
        _ -> IdentifierToken identifier : tokenize rest

tokenizeString :: String -> [Token]
tokenizeString xs =
  let (string, rest) = span (/= '"') xs
   in StringToken string : tokenize (tail rest)

tokenizeOperator :: String -> [Token]
tokenizeOperator xs =
  let (operator, rest) = span isOperator xs
   in OperatorToken operator : tokenize rest

isOperator :: Char -> Bool
isOperator x = x == '+' || x == '-' || x == '*' || x == '/'