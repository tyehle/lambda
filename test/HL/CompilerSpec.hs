module HL.CompilerSpec (compilerTests) where

import HL.Compiler
import HL.AST
import HL.Base (readBase)
import HL.Parser (parseProgram)
import Node
import Interpreter
import Extraction (Extractor, runExtractor, intExtractor, listExtractor, boolExtractor)

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

testWithDefs :: (Eq a, Show a) => (Exp -> Either String a) -> Integer -> String -> a -> TestTree
testWithDefs reduce n input expected = testCase (show n) assertion
  where
    assertion = do
      defs <- readBase
      let expr = desugarDefs <$> defs <*> parseProgram "test input" input
      either assertFailure (@?= expected) (expr >>= reduce)

test :: Integer -> String -> Node -> TestTree
test = testWithDefs (Right . compileExp)

testRun :: Integer -> String -> Node -> TestTree
testRun = testWithDefs (interp . compileExp)

testExtract :: (Eq a, Show a) => Integer -> Extractor a -> String -> a -> TestTree
testExtract n ex = testWithDefs ((>>= runExtractor ex) . interp . compileExp) n


variableTests :: TestTree
variableTests = testGroup "Variable Tests"
  [ test 1 "x" (Ref "x")
  ]

boolTests :: TestTree
boolTests = testGroup "Bool Tests"
  [ test 1 "#t" $ Lam "t" $ Lam "f" $ Ref "t"
  , test 2 "#f" $ Lam "t" $ Lam "f" $ Ref "f"

  , testGroup "If Tests"
      [ test 1 "(if #f a b)" $
          (false `App` Lam "y" (Ref "a") `App` Lam "y" (Ref "b")) `App` Lam "x" (Ref "x")
      , testRun 2 "(if #t (λ (i) i) x)" $ Lam "i" (Ref "i")
      , testRun 3 "(if #f x (λ (i) i))" $ Lam "i" (Ref "i")
      ]

  , testGroup "And Tests"
      [ testRun 1 "(and #t #f)" false
      , testRun 2 "(and #f x)" false
      ]

  , testGroup "Or Tests"
      [ testRun 1 "(or #f #t)" true
      , testRun 2 "(or #t x)" true
      ]

  , testGroup "Not Tests"
      [ testRun 1 "(not #f)" true
      , testRun 2 "(not #t)" false
      ]
  ]

numTests :: TestTree
numTests = testGroup "Num Tests"
  [ test 1 "0" $ Lam "f" $ Lam "x" $ Ref "x"
  , test 2 "2" $ Lam "f" $ Lam "x" $ Ref "f" `App` (Ref "f" `App` Ref "x")

  , testGroup "IsZero Tests"
    [ testRun 1 "(zero? 0)" true
    , testRun 2 "(zero? 42)" false
    , test 3 "(zero? 0)" $ Lam "f" (Lam "x" (Ref "x")) `App` Lam "x" false `App` true
    ]

  , testGroup "Eq Tests"
    [ testRun 1 "(= 1 0)" false
    , testRun 2 "(= 0 1)" false
    , testRun 3 "(= 2 2)" true
    ]

  , testGroup "IsEven Tests"
      [ testRun 1 "(even? 0)" true
      , testRun 2 "(even? 1)" false
      , testRun 3 "(even? 42)" true
      , test 4 "(even? 1)" $ Lam "f" (Lam "x" (Ref "f" `App` Ref "x"))
          `App` Lam "x" (Ref "x" `App` false `App` true)
          `App` true
      ]

  , testGroup "Plus Tests"
      [ testExtract 1 intExtractor "(+ 2 3)" 5
      , testExtract 2 intExtractor "(+ 0 2)" 2
      , testExtract 3 intExtractor "(+ 3 0)" 3
      ]

  , testGroup "Mult Tests"
      [ testExtract 1 intExtractor "(* 2 3)" 6
      , testExtract 2 intExtractor "(* 0 2)" 0
      , testExtract 3 intExtractor "(* 3 0)" 0
      , testExtract 4 intExtractor "(* 1 5)" 5
      ]

  , testGroup "Minus Tests"
      [ testExtract 1 intExtractor "(- 2 3)" 0
      , testExtract 2 intExtractor "(- 3 2)" 1
      , testExtract 3 intExtractor "(- 5 5)" 0
      ]

  , testGroup "Divide Tests"
      [ testExtract 1 intExtractor "(/ 1 2)" 0
      , testExtract 2 intExtractor "(/ 2 2)" 1
      , testExtract 3 intExtractor "(/ 3 2)" 1
      , testExtract 4 intExtractor "(/ 4 2)" 2
      ]
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ test 1 "(λ (a b) (a b))" $ Lam "a" $ Lam "b" $ Ref "a" `App` Ref "b"
  , testRun 2 "((λ (a b) b) c (λ (i) i))" $ Lam "i" (Ref "i")
  , test 3 "(let [] (λ (i) i))" $ Lam "i" (Ref "i")
  , test 4 "(let [(x #t)] x)" $ Lam "x" (Ref "x") `App` true
  , test 5 "(let [(x #t) (y #f)] x)" $
      Lam "x" (Lam "y" (Ref "x") `App` false) `App` true
  , testRun 6 "(let [(x #t) (y #f)] x)" true
  , testRun 7 "(letrec (f (λ (a) (if a #t (f (not a))))) (f #f))" true
  ]


listTests :: TestTree
listTests = testGroup "List Tests"
  [ testExtract 1 (listExtractor intExtractor) "(cons 0 empty)" [0]
  , testExtract 2 (listExtractor intExtractor) "(cons 0 (cons 1 empty))" [0, 1]
  , testExtract 3 (listExtractor intExtractor) "empty" []
  , testExtract 4 intExtractor "(head (cons 0 empty))" 0
  , testExtract 5 (listExtractor intExtractor) "(tail (cons 0 empty))" []
  , testExtract 6 boolExtractor "(pair? empty)" False
  , testExtract 7 boolExtractor "(pair? (cons #f empty))" True
  , testExtract 8 boolExtractor "(null? empty)" True
  , testExtract 9 boolExtractor "(null? (cons #f empty))" False
  ]


appTests :: TestTree
appTests = testGroup "Application Tests"
  [ test 1 "(a b)" $ Ref "a" `App` Ref "b"
  , testRun 2 "((λ (x) x) #t)" true
  ]
