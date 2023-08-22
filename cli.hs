import Lexer
import Parser
import Interpreter
import System.Environment
import System.Exit

main = do
    args <- getArgs
    source <- parseCli args
    let tokens = tokenize source
    let ast = parse tokens
    let result = evaluate ast
    print result

parseCli ["-h"] = usage >> exit
parseCli ["-v"] = version >> exit
parseCli [] = getContents
parseCli fs = concat `fmap` mapM readFile fs

usage = putStrLn "Usage: tac [-vh] [file ..]"

version = putStrLn "Haskell tac 0.1"

exit = exitWith ExitSuccess

die = exitWith (ExitFailure 1)
