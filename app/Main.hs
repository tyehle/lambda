module Main where

import Node (Node)
import Extraction (extractInt, Extractor, runExtractor)
import Interpreter (interp)
import HL.Compiler (compile)
import HL.Parser (parseProgram)


main :: IO ()
main = either putStrLn print $ extractInt =<< pipeline program
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"

pipeline :: String -> Either String Node
pipeline program = parseProgram "input" program >>= compile >>= interp

runFile :: Show a => String -> Extractor a -> IO ()
runFile filename extractor = do
  input <- readFile filename
  either putStrLn print $ pipeline input >>= runExtractor extractor
