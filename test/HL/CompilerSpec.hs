module HL.CompilerSpec (compilerTests) where

import HL.Compiler
import HL.AST
import HL.Base (readBase)
import HL.Typed (parseProgram)
import HL.TypeInference (inferProgram)
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
  , listTests
  , appTests
  ]

testWithDefs :: (Eq a, Show a) => (Exp -> Either String a) -> Integer -> String -> a -> TestTree
testWithDefs reduce n input expected = testCase (show n) assertion
  where
    assertion = do
      defs <- readBase
      let expr = desugarDefs <$> defs <*> (parseProgram "test input" input >>= inferProgram)
      either assertFailure (@?= expected) (expr >>= reduce)

test :: Integer -> String -> Node -> TestTree
test = testWithDefs (Right . compileExp)

testRun :: Integer -> String -> Node -> TestTree
testRun = testWithDefs (interp . compileExp)

testExtract :: (Eq a, Show a) => Integer -> (Node -> Either String a) -> String -> a -> TestTree
testExtract n ex = testWithDefs (ex . compileExp) n


true :: Node
true = Lam "f" $ Lam "t" $ Ref "t"

false :: Node
false = Lam "f" $ Lam "t" $ Ref "f"


variableTests :: TestTree
variableTests = testGroup "Variable Tests"
  [ test 1 "x" (Ref "x")
  ]

boolTests :: TestTree
boolTests = testGroup "Bool Tests"
  [ testRun 1 "#t" true
  , testRun 2 "#f" false

  , testGroup "If Tests"
      [ testRun 1 "(if #t (λ (i) i) x)" $ Lam "i" (Ref "i")
      , testRun 2 "(if #f x (λ (i) i))" $ Lam "i" (Ref "i")
      , testRun 3 "(if (id #t) (λ (i) i) x)" $ Lam "i" (Ref "i")
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
      ]

  , testGroup "Plus Tests"
      [ testExtract 1 extractInt "(+ 2 3)" 5
      , testExtract 2 extractInt "(+ 0 2)" 2
      , testExtract 3 extractInt "(+ 3 0)" 3
      ]

  , testGroup "Mult Tests"
      [ testExtract 1 extractInt "(* 2 3)" 6
      , testExtract 2 extractInt "(* 0 2)" 0
      , testExtract 3 extractInt "(* 3 0)" 0
      , testExtract 4 extractInt "(* 1 5)" 5
      ]

  , testGroup "Minus Tests"
      [ testExtract 1 extractInt "(- 2 3)" 0
      , testExtract 2 extractInt "(- 3 2)" 1
      , testExtract 3 extractInt "(- 5 5)" 0
      ]

  , testGroup "Divide Tests"
      [ testExtract 1 extractInt "(/ 1 2)" 0
      , testExtract 2 extractInt "(/ 2 2)" 1
      , testExtract 3 extractInt "(/ 3 2)" 1
      , testExtract 4 extractInt "(/ 4 2)" 2
      ]
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda Tests"
  [ test 1 "(λ (a b) (a b))" $ Lam "a" $ Lam "b" $ Ref "a" `App` Ref "b"
  , testRun 2 "((λ (a b) b) c (λ (i) i))" $ Lam "i" (Ref "i")
  , test 3 "(let [] (λ (i) i))" $ Lam "i" (Ref "i")
  , test 4 "(let [(x u)] x)" $ Lam "x" (Ref "x") `App` Ref "u"
  , test 5 "(let [(x u) (y v)] x)" $
      Lam "x" (Lam "y" (Ref "x") `App` Ref "v") `App` Ref "u"
  , testRun 6 "(let [(x #t) (y #f)] x)" true
  , testRun 7 "(letrec (f (λ (a) (if a #t (f (not a))))) (f #f))" true
  ]


listTests :: TestTree
listTests = testGroup "List Tests"
  [ testExtract 1 (extractList intExtractor) "(Cons 0 Empty)" [0]
  , testExtract 2 (extractList intExtractor) "(Cons 0 (Cons 1 Empty))" [0, 1]
  , testExtract 3 (extractList intExtractor) "Empty" []
  , testExtract 4 extractInt "(head (Cons 0 Empty))" 0
  , testExtract 5 (extractList intExtractor) "(tail (Cons 0 Empty))" []
  , testExtract 6 extractBool "(pair? Empty)" False
  , testExtract 7 extractBool "(pair? (Cons #f Empty))" True
  , testExtract 8 extractBool "(null? Empty)" True
  , testExtract 9 extractBool "(null? (Cons #f Empty))" False
  ]


appTests :: TestTree
appTests = testGroup "Application Tests"
  [ test 1 "(a b)" $ Ref "a" `App` Ref "b"
  , testRun 2 "((λ (x) x) #t)" true
  ]
