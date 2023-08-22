module Interpreter (evaluate, Expression (IntValue, BoolValue, StringValue, ListValue, Operator, Sequence, EmptyExpression)) where

type Operand = String

data Expression = IntValue Int | BoolValue Bool | StringValue String | ListValue [Expression] | Operator Operand Expression Expression | Sequence [Expression] | EmptyExpression
  deriving (Show, Eq)

evaluate :: Expression -> Expression
evaluate expr =
  case expr of
    IntValue x -> IntValue x
    BoolValue x -> BoolValue x
    StringValue x -> StringValue x
    ListValue x -> ListValue x
    Sequence x -> evaluateSequence x
    Operator op left right -> evaluateOperator op left right

evaluateSequence :: [Expression] -> Expression
evaluateSequence [] = IntValue 0
evaluateSequence [x] = evaluate x
evaluateSequence (x : xs) =
  let _ = evaluate x
   in evaluateSequence xs

evaluateOperator :: Operand -> Expression -> Expression -> Expression
evaluateOperator op left right =
  let leftValue = evaluate left
      rightValue = evaluate right
   in -- check whether they evaluate to integers
      case (leftValue, rightValue) of
        (IntValue leftInt, IntValue rightInt) ->
          case op of
            "+" -> IntValue (leftInt + rightInt)
            "-" -> IntValue (leftInt - rightInt)
            "*" -> IntValue (leftInt * rightInt)
            "/" -> IntValue (leftInt `div` rightInt)
            _ -> error "Invalid operator"
        _ -> error "Invalid operands"