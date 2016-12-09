module HL.CompilerSpec (compilerTests) where

import HL.Compiler
import HL.AST
import Node

import Test.Tasty
import Test.Tasty.HUnit

compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ variableTests
  , boolTests
  , numTests
  , lambdaTests
  ]

test :: Integer -> Exp -> Node -> TestTree
test n input expected = testCase (show n) $ compile input @?= expected

variableTests :: TestTree
variableTests = testGroup "Variable Tests"
  [ test 1 (Var "x") (Ref "x")
  ]

boolTests :: TestTree
boolTests = testGroup "Bool Tests"
  [ test 1 VTrue $ Lam "t" $ Lam "f" $ Ref "t"
  , test 2 VFalse $ Lam "t" $ Lam "f" $ Ref "f"
  , test 3 (If VFalse (Var "a") (Var "b")) $
      (false `App` Lam "y" (Ref "a") `App` Lam "y" (Ref "b")) `App` Lam "x" (Ref "x")
  ]

numTests :: TestTree
numTests = testGroup "Num Tests"
  [ test 1 (Num 0) $ Lam "f" $ Lam "x" $ Ref "x"
  , test 2 (Num 2) $ Lam "f" $ Lam "x" $ Ref "f" `App` (Ref "f" `App` Ref "x")
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ test 1 (Lambda ["a", "b"] (Var "a" `Application` Var "b")) $ Lam "a" $ Lam "b" $ Ref "a" `App` Ref "b"
  ]
