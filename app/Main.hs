module Main where

import Node (Node)
import Pretty
import Interpreter (interp, extractInt)
import HL.Compiler (compile)
import HL.Parser (parseProgram)
import HL.Base (readBase)


main :: IO ()
main = runProgram program extractInt
  where
    program = "(define (f x)            \
              \  (if (zero? x)          \
              \      1                  \
              \      (* x (f (- x 1)))))\
              \(f 5)                    "

prettyProgram :: String -> IO ()
prettyProgram input = runDisplayProgram (putStrLn . pretty) "input" input interp

runDisplayProgram :: (a -> IO ()) -> String -> String -> (Node -> Either String a) -> IO ()
runDisplayProgram display filename input extractor = do
  base <- readBase
  either putStrLn display $ do
    defs <- base
    prog <- parseProgram filename input
    compiled <- compile defs prog
    extractor compiled

runProgram :: Show a => String -> (Node -> Either String a) -> IO ()
runProgram = runDisplayProgram print "input"

runFile :: Show a => String -> (Node -> Either String a) -> IO ()
runFile filename extractor = do
  input <- readFile filename
  runDisplayProgram print filename input extractor
