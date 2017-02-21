module Main where

import Node (Node)
import Interpreter (extractInt)
import HL.Compiler (compile, checkScope, compileExp, desugarDefs)
import HL.Parser (parseProgram)
import HL.Base (readBase)


main :: IO ()
main = either putStrLn print $ pipeline program extractInt
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"

pipeline :: String -> (Node -> Either String a) -> Either String a
pipeline program extract = parseProgram "input" program >>= compile >>= extract

runFile :: Show a => String -> (Node -> Either String a) -> IO ()
runFile filename extractor = do
  input <- readFile filename
  base <- readBase
  either putStrLn print $ do
    defs <- base
    prog <- parseProgram filename input
    compiled <- (checkScope . compileExp . desugarDefs defs) prog
    extractor compiled
