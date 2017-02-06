module HL.CompilerSpec (compilerTests) where

import HL.Compiler
import HL.AST
import Node
import Interpreter
import Extraction (Extractor, runExtractor, intExtractor)

import Test.Tasty
import Test.Tasty.HUnit

compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ variableTests
  , boolTests
  , numTests
  , lambdaTests
  , listTests
  , appTests
  ]

test :: Integer -> Exp -> Node -> TestTree
test n input expected = testCase (show n) $ compileExp input @?= expected

testRun :: Integer -> Exp -> Node -> TestTree
testRun n input expected = testCase (show n) assertion
  where
    assertion = (interp . compileExp) input @?= Right expected

testExtract :: (Eq a, Show a) => Integer -> Extractor a -> Exp -> a -> TestTree
testExtract n ex input expected = testCase (show n) assertion
  where
    assertion = ((interp . compileExp) input >>= runExtractor ex) @?= Right expected

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

  , testGroup "If Tests"
      [ test 1 (If VFalse (Var "a") (Var "b")) $
          (false `App` Lam "y" (Ref "a") `App` Lam "y" (Ref "b")) `App` Lam "x" (Ref "x")
      , testRun 2 (If VTrue idE (Var "x")) idN
      , testRun 3 (If VFalse (Var "x") idE) idN
      ]

  , testGroup "And Tests"
      [ testRun 1 (And VTrue VFalse) false
      , testRun 2 (And VFalse (Var "x")) false
      ]

  , testGroup "Or Tests"
      [ testRun 1 (Or VFalse VTrue) true
      , testRun 2 (Or VTrue (Var "x")) true
      ]

  , testGroup "Not Tests"
      [ testRun 1 (Not VFalse) true
      , testRun 2 (Not VTrue) false
      ]
  ]

numTests :: TestTree
numTests = testGroup "Num Tests"
  [ test 1 (Num 0) $ Lam "f" $ Lam "x" $ Ref "x"
  , test 2 (Num 2) $ Lam "f" $ Lam "x" $ Ref "f" `App` (Ref "f" `App` Ref "x")

  , testGroup "IsZero Tests"
    [ testRun 1 (IsZero (Num 0)) true
    , testRun 2 (IsZero (Num 42)) false
    , test 3 (IsZero (Num 0)) $ Lam "f" (Lam "x" (Ref "x")) `App` Lam "x" false `App` true
    ]

  , testGroup "Eq Tests"
    [ testRun 1 (Eq (Num 1) (Num 0)) false
    , testRun 2 (Eq (Num 0) (Num 1)) false
    , testRun 3 (Eq (Num 2) (Num 2)) true
    ]

  , testGroup "IsEven Tests"
      [ testRun 1 (IsEven (Num 0)) true
      , testRun 2 (IsEven (Num 1)) false
      , testRun 3 (IsEven (Num 42)) true
      , test 4 (IsEven (Num 1)) $ Lam "f" (Lam "x" (Ref "f" `App` Ref "x"))
          `App` Lam "x" (Ref "x" `App` false `App` true)
          `App` true
      ]

  , testGroup "Plus Tests"
      [ testExtract 1 intExtractor (Plus (Num 2) (Num 3)) 5
      , testExtract 2 intExtractor (Plus (Num 0) (Num 2)) 2
      , testExtract 3 intExtractor (Plus (Num 3) (Num 0)) 3
      ]

  , testGroup "Mult Tests"
      [ testExtract 1 intExtractor (Mult (Num 2) (Num 3)) 6
      , testExtract 2 intExtractor (Mult (Num 0) (Num 2)) 0
      , testExtract 3 intExtractor (Mult (Num 3) (Num 0)) 0
      , testExtract 4 intExtractor (Mult (Num 1) (Num 5)) 5
      ]

  , testGroup "Minus Tests"
      [ testExtract 1 intExtractor (Minus (Num 2) (Num 3)) 0
      , testExtract 2 intExtractor (Minus (Num 3) (Num 2)) 1
      , testExtract 3 intExtractor (Minus (Num 5) (Num 5)) 0
      ]

  , testGroup "Divde Tests"
      [ testExtract 1 intExtractor (Divide (Num 1) (Num 2)) 0
      , testExtract 2 intExtractor (Divide (Num 2) (Num 2)) 1
      , testExtract 3 intExtractor (Divide (Num 3) (Num 2)) 1
      , testExtract 4 intExtractor (Divide (Num 4) (Num 2)) 2
      ]
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ test 1 (Lambda ["a", "b"] (Var "a" `Application` Var "b")) $ Lam "a" $ Lam "b" $ Ref "a" `App` Ref "b"
  , test 2 (Let [] idE) idN
  , test 3 (Let [("x", VTrue)] (Var "x")) $ Lam "x" (Ref "x") `App` true
  , test 4 (Let [("x", VTrue), ("y", VFalse)] (Var "x")) $
      Lam "x" (Lam "y" (Ref "x") `App` false) `App` true
  , testRun 5 (Let [("x", VTrue), ("y", VFalse)] (Var "x")) true
  ]

listTests :: TestTree
listTests = testGroup "List Tests"
  []


appTests :: TestTree
appTests = testGroup "Application Tests"
  []
