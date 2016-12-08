module Main where

import Lambda

main :: IO ()
main = do
  -- (\a b -> b) (\x -> x) (\y -> y)
  print . interp [] $ App (App (Lambda "a" (Lambda "b" (Ref "b"))) (Lambda "x" (Ref "x"))) (Lambda "y" (Ref "y"))
  -- (\a b -> a) (\x -> x) (\y -> y)
  print . interp [] $ App (App (Lambda "a" (Lambda "b" (Ref "a"))) (Lambda "x" (Ref "x"))) (Lambda "y" (Ref "y"))
