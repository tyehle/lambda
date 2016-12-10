module Main where

import Extraction (extractInt)
import Interpreter (interp)
import HL.Compiler (compile)
import HL.Parser (parseHL)

main :: IO ()
main = either print print . extractInt . interp . compile . parseHL $ program
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"
