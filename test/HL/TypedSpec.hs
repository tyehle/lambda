module HL.TypedSpec (typedTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec (parse)

import HL.Typed
import HL.SExp

typedTests :: TestTree
typedTests = testGroup "Typed Tests"
  [ identTests
  , numTests
  , caseTests
  , letrecTests
  , letTests
  , lambdaTests
  , eannTests
  , exprTests
  ]

testFromString :: (Eq a, Show a) => Int -> (SExp -> a) -> String -> a -> TestTree
testFromString n f input expected = testCase (show n) assertion
  where
    test = parse sExp "test input" input
    assertion = either (assertFailure . show) ((@?= expected) . f) test

identTests :: TestTree
identTests = testGroup "Ident Tests"
  [ testFromString 1 toIdent "foo" $ Right "foo"
  , testFromString 2 toIdent "->" $ Right "->"
  , testFromString 3 toIdent "Two2" $ Right "Two2"
  , testFromString 4 toIdent "_" $ Left "Invalid identifier _"
  , testFromString 5 toArg "_" $ Right "_"
  ]

numTests :: TestTree
numTests = testGroup "Num Tests"
  [ testFromString 1 toNum "42" $ Right (Num 42)
  , testFromString 2 toNum "0" $ Right (Num 0)
  , testFromString 3 toNum "-0" $ Right (Num 0)
  , testFromString 4 toNum "foo" $ Left "Invalid number foo"
  -- , testFromString 5 toNum "-12" $ Left "Invalid number -12"
  ]

caseTests :: TestTree
caseTests = testGroup "Case Tests"
  [ testFromString 1 toCase "(case x [(Just _) 1] [Nothing 0])" $
      Right (Case (Var "x") [("Just", ["_"], Num 1), ("Nothing", [], Num 0)])
  , testFromString 2 toCase "(case x)" $ Left "Invalid case (case x)"
  , testFromString 3 toCase "(case x [() 1])" $ Left "Invalid pattern ()"
  ]

letrecTests :: TestTree
letrecTests = testGroup "Letrec Tests"
  [ testFromString 1 toLetrec "(letrec (f g) 42)" $ Right (Letrec "f" (Var "g") (Num 42))
  , testFromString 2 toLetrec "(letrec (x) 666)" $ Left "Invalid letrec (letrec (x) 666)"
  ]

letTests :: TestTree
letTests = testGroup "Let Tests"
  [ testFromString 1 toLet "(let [(x 1)] x)" $ Right (Let [("x", Num 1)] (Var "x"))
  , testFromString 2 toLet "(let x 5 x)" $ Left "Invalid let (let x 5 x)"
  , testFromString 3 toLet "(let [(x 1) (y 2 3)] x)" $ Left "Invalid binding (y 2 3)"
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ testFromString 1 toLambda "(lambda (x) x)" $ Right (Lambda ["x"] (Var "x"))
  , testFromString 2 toLambda "(λ (x y) y)" $ Right (Lambda ["x", "y"] (Var "y"))
  , testFromString 3 toLambda "(λ x x)" $ Left "Invalid lambda (λ x x)"
  ]

eannTests :: TestTree
eannTests = testGroup "EAnn Tests"
  [ testFromString 1 toEAnn "(type x Num)" $ Right (EAnn (Var "x") (Leaf "Num"))
  , testFromString 2 toEAnn "(type asdf)" $ Left "Invalid type annotation (type asdf)"
  ]

exprTests :: TestTree
exprTests = testGroup "Exp Tests"
  [ testFromString 1 toExpr "42" $ Right (Num 42)
  , testFromString 2 toExpr "-12" $ Left "Invalid number -12"
  , testFromString 3 toExpr "-" $ Right (Var "-")
  , testFromString 4 toExpr "(f x)" $ Right (Application (Var "f") (Var "x"))
  , testFromString 5 toExpr "(f)" $ Right (Var "f")
  , testFromString 6 toExpr "()" $ Left "Illegal empty expression"
  ]
