module Main where

import Node (Node)
import Extraction (extractInt, Extractor, runExtractor)
import Interpreter (interp)
import HL.Compiler (compile, checkScope, compileExp, desugarDefs)
import HL.Parser (parseProgram)
import HL.Base (readBase)


main :: IO ()
main = either putStrLn print $ extractInt =<< pipeline program
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"

pipeline :: String -> Either String Node
pipeline program = parseProgram "input" program >>= compile >>= interp

runFile :: Show a => String -> Extractor a -> IO ()
runFile filename extractor = do
  input <- readFile filename
  base <- readBase
  either putStrLn print $ do
    defs <- base
    prog <- parseProgram filename input
    compiled <- (checkScope . compileExp . desugarDefs defs) prog
    reduced <- interp compiled
    runExtractor extractor reduced
