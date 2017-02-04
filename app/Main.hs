module Main where

import Node (Node)
import Extraction (extractInt)
import Interpreter (errInterp)
import HL.Compiler (errCompile)
import HL.Parser (parseHL)
import HL.Scoper (scopeExp)

main :: IO ()
main = either putStrLn print $ extractInt =<< pipeline program
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"

pipeline :: String -> Either String Node
pipeline program = parseHL program >>= scopeExp >>= errCompile >>= errInterp
