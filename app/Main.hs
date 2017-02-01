module Main where

import Extraction (extractInt)
import Interpreter (errInterp)
import HL.Compiler (errCompile)
import HL.Parser (parseHL)

main :: IO ()
-- main = either putStrLn print $ extractInt =<< interp . compile <$> parseHL program
main = either putStrLn print $ parseHL program >>= errCompile >>= errInterp >>= extractInt
  where
    program = "(letrec (f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))) (f 5))"
