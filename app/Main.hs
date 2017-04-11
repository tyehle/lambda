module Main where

import Node (Node)
import Pretty
import Interpreter (interp, extractInt)
import HL.Compiler (compile)
import HL.Typed (parseProgram)
import HL.TypeInference (inferProgram)
import HL.Base (readBase)


type Interp a = Node -> Either String a

main :: IO ()
main = runProgram program extractInt
  where
    program = "(define (fact x)            \
              \  (if (zero? x)             \
              \      1                     \
              \      (* x (fact (- x 1)))))\
              \(fact 5)                    "

prettyProgram :: String -> IO ()
prettyProgram prog = runDisplayProgram (putStrLn . pretty) "input" prog interp

runDisplayProgram :: (a -> IO ()) -> String -> String -> Interp a -> IO ()
runDisplayProgram display filename input extractor = do
  base <- readBase
  either putStrLn display $ do
    defs <- base
    prog <- parseProgram filename input >>= inferProgram
    compiled <- compile defs prog
    extractor compiled

runProgram :: Show a => String -> Interp a -> IO ()
runProgram = runDisplayProgram print "input"

runFile :: Show a => String -> Interp a -> IO ()
runFile filename extractor = do
  input <- readFile filename
  runDisplayProgram print filename input extractor
