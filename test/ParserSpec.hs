module ParserSpec (parserTests) where

import Node
import Parser

import Text.Parsec
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ applicationTests
  , parenTests
  , lambdaTests
  , identifierTests
  ]

test :: Integer -> String -> Either ParseError Node -> TestTree
test n input expected = testCase (show n) assertion
  where
    assertion = parse program "test-input" input @?= expected

applicationTests :: TestTree
applicationTests = testGroup "Application Tests"
  [ test 1 "x y z" $ Right (App (App (Ref "x") (Ref "y")) (Ref "z"))
  , test 2 "x (y z)" $ Right (App (Ref "x") (App (Ref "y") (Ref "z")))
  , test 3 "x " $ Right (Ref "x")
  , test 4 " x" $ Right (Ref "x")
  , test 5 " ( x \n y ) " $ Right $ App (Ref "x") (Ref "y")
  ]

parenTests :: TestTree
parenTests = testGroup "Paren Tests"
  [ test 1 "((x))" $ Right $ Ref "x"
  , test 2 " (  (  x )  ) " $ Right $ Ref "x"
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ test 1 "\\x.x" $ Right $ Lambda "x" $ Ref "x"
  , test 2 " \\ x . x " $ Right $ Lambda "x" $ Ref "x"
  , test 3 "\\x y. x y" $ Right $ Lambda "x" $ Lambda "y" $ App (Ref "x") (Ref "y")
  ]

identifierTests :: TestTree
identifierTests = testGroup "Identifier Tests"
  [ test 1 "x23T" $ Right (Ref "x23T")
  ]
