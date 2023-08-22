import Interpreter

main = do
  let x = evaluate (Operator "+" (IntValue 5) (IntValue 2))
  print x
