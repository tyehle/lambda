module InterpreterSpec (interpTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser (parseNode)
import Interpreter

test :: Integer -> String -> String -> TestTree
test n input expected = testCase (show n) assertion
  where
    assertion = interp (parseNode input) @?= Right (parseNode expected)

interpTests :: TestTree
interpTests = testGroup "Interp Tests"
  [ test 1 "(\\a a . a) \\x.x" "\\a.a"
  , test 2 "(\\a b . a) (\\x.x) z" "\\x.x"
  , test 3 "\\x.x" "\\x.x"
  , test 4 "(\\a.a a) \\x.x" "\\x.x"
  ]
