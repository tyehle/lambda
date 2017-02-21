module Main where

import Node (Node)
import Interpreter (extractInt)
import HL.Compiler (compile)
import HL.Parser (parseProgram)
import HL.Base (readBase)


main :: IO ()
main = runProgram "input" program extractInt
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"

runProgram :: Show a => String -> String -> (Node -> Either String a) -> IO ()
runProgram filename input extractor = do
  base <- readBase
  either putStrLn print $ do
    defs <- base
    prog <- parseProgram filename input
    compiled <- compile defs prog
    extractor compiled

runFile :: Show a => String -> (Node -> Either String a) -> IO ()
runFile filename extractor = do
  input <- readFile filename
  runProgram filename input extractor
