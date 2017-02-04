module Main where

import Node (Node)
import Extraction (extractInt, Extractor, runExtractor)
import Interpreter (errInterp)
import HL.Compiler (compile)
import HL.Parser (parseHL)
import HL.Scoper (scopeProgram)

main :: IO ()
main = either putStrLn print $ extractInt =<< pipeline program
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"

pipeline :: String -> Either String Node
pipeline program = parseHL program >>= scopeProgram >>= compile >>= errInterp

runFile :: Show a => String -> Extractor a -> IO ()
runFile filename extractor = do
  input <- readFile filename
  let result = pipeline input
  either putStrLn print $ result >>= runExtractor extractor
