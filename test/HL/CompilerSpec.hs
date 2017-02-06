module HL.CompilerSpec (compilerTests) where

import HL.Compiler
import HL.AST
import Node
import Interpreter

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
test n input expected = testCase (show n) $ compileExp input @?= expected

testRun :: Integer -> Exp -> Node -> TestTree
testRun n input expected = testCase (show n) assertion
  where
    assertion = (interp . compileExp) input @?= Right expected

idE :: Exp
idE = Lambda ["i"] $ Var "i"
idN :: Node
idN = Lam "i" $ Ref "i"

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
  , testRun 4 (If VTrue idE (Var "x")) idN
  , testRun 5 (If VFalse (Var "x") idE) idN
  , testRun 7 (And VTrue VFalse) false
  , testRun 8 (And VFalse (Var "x")) false
  , testRun 9 (Or VFalse VTrue) true
  , testRun 10 (Or VTrue (Var "x")) true
  , testRun 11 (Not VFalse) true
  , testRun 12 (Not VTrue) false
  ]

numTests :: TestTree
numTests = testGroup "Num Tests"
  [ test 1 (Num 0) $ Lam "f" $ Lam "x" $ Ref "x"
  , test 2 (Num 2) $ Lam "f" $ Lam "x" $ Ref "f" `App` (Ref "f" `App` Ref "x")
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ test 1 (Lambda ["a", "b"] (Var "a" `Application` Var "b")) $ Lam "a" $ Lam "b" $ Ref "a" `App` Ref "b"
  , test 2 (Let [] (Lambda ["x"] (Var "x"))) $ Lam "x" $ Ref "x"
  , test 3 (Let [("x", VTrue)] (Var "x")) $ Lam "x" (Ref "x") `App` true
  , test 4 (Let [("x", VTrue), ("y", VFalse)] (Var "x")) $
      Lam "x" (Lam "y" (Ref "x") `App` false) `App` true
  , testRun 5 (Let [("x", VTrue), ("y", VFalse)] (Var "x")) true
  ]
