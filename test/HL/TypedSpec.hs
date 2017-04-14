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
  , typeTests
  , qtypeTests
  , structTests
  , annTests
  , defTests
  , fileTests
  ]

testFromString :: (Eq a, Show a) => Int -> (SExp -> a) -> String -> a -> TestTree
testFromString n f input expected = testCase (show n) assertion
  where
    parsed = parse sExp "test input" input
    assertion = either (assertFailure . show) ((@?= expected) . f) parsed

identTests :: TestTree
identTests = testGroup "Ident Tests"
  [ testFromString 1 toIdent "foo" $ Right "foo"
  , testFromString 2 toIdent "->" $ Right "->"
  , testFromString 3 toIdent "Two2" $ Right "Two2"
  , testFromString 4 toIdent "case" $ Left "Invalid identifier case"
  , testFromString 5 toIdent "_" $ Left "Invalid identifier _"
  , testFromString 6 toIdent "_foo" $ Left "Invalid identifier _foo"
  , testFromString 7 toArg "_" $ Right "_"
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

typeTests :: TestTree
typeTests = testGroup "Type Tests"
  [ testFromString 1 toType "->" $ Right (Leaf "->")
  , testFromString 2 toType "12" $ Left "Invalid identifier 12"
  , testFromString 3 toType "()" $ Right (Node [])
  , testFromString 4 toType "(List a)" $ Right (Node [Leaf "List", Leaf "a"])
  ]

qtypeTests :: TestTree
qtypeTests = testGroup "Qualified Type Tests"
  [ testFromString 1 toQType "Bool" $ Right (Forall [] (Leaf "Bool"))
  , testFromString 2 toQType "12" $ Left "Invalid identifier 12"
  , testFromString 3 toQType "(List a)" $ Right (Forall [] (Node [Leaf "List", Leaf "a"]))
  , testFromString 4 toQType "(forall a a)" $ Right (Forall ["a"] (Leaf "a"))
  , testFromString 5 toQType "(V (a) Bool)" $ Right (Forall ["a"] (Leaf "Bool"))
  , testFromString 6 toQType "(∀ (a b) (-> a b))" $
      Right (Forall ["a", "b"] (Node [Leaf "->", Leaf "a", Leaf "b"]))
  , testFromString 7 toQType "(∀ V V)" $ Left "Invalid identifier V"
  ]

structTests :: TestTree
structTests = testGroup "Struct Tests"
  [ testFromString 1 toStruct "(struct Bool [True] [False])" $
      Right (Struct (Forall [] (Leaf "Bool")) [Leaf "True", Leaf "False"])
  , testFromString 2 toStruct "(struct (∀ a (List a)) [Cons a (List a)] [Nil]))" $
      Right (Struct
              (Forall ["a"] (Node [Leaf "List", Leaf "a"]))
              [ Node [Leaf "Cons", Leaf "a", Node [Leaf "List", Leaf "a"]]
              , Leaf "Nil"])
  , testFromString 3 toStruct "(struct A)" $ Left "Invalid struct (struct A)"
  , testFromString 4 toStruct "(struct A [])" $ Left "Invalid variant ()"
  , testFromString 5 toStruct "(struct A [(Foo a) b])" $
      Left "Invalid variant ((Foo a) b)"
  , testFromString 6 toStruct "(struct A True False)" $
      Left "Invalid variant True"
  ]

annTests :: TestTree
annTests = testGroup "Annotation Tests"
  [ testFromString 1 toAnn "(type foo A)" $ Right (TAnn "foo" (Forall [] (Leaf "A")))
  , testFromString 2 toAnn "(type id (∀ a (-> a a)))" $
      Right (TAnn "id" (Forall ["a"] (Node [Leaf "->", Leaf "a", Leaf "a"])))
  , testFromString 3 toAnn "(type foo)" $ Left "Invalid type annotation (type foo)"
  ]

defTests :: TestTree
defTests = testGroup "Def Tests"
  [ testFromString 1 toDef "(define (f x) x)" $ Right (Def "f" (Lambda ["x"] (Var "x")))
  , testFromString 2 toDef "(define (f) 1)" $ Left "Invalid identifier (f)"
  , testFromString 3 toDef "(define f 1)" $ Right (Def "f" (Num 1))
  , testFromString 4 toDef "(define f)" $ Left "Invalid definition (define f)"
  ]

testFileFromString :: (Eq a, Show a) => Int -> ([SExp] -> a) -> String -> a -> TestTree
testFileFromString n f input expected = testCase (show n) assertion
  where
    parsed = parse fileParser "test input" input
    assertion = either (assertFailure . show) ((@?= expected) . f) parsed

fileTests :: TestTree
fileTests = testGroup "File Tests"
  [ testFileFromString 1 toModule "(define x 1)" $ Right [Def "x" (Num 1)]
  , testFileFromString 2 toModule "(define x 1) x" $ Left "Invalid definition x"
  , testFileFromString 3 toProgram "x" $ Right (Program [] (Var "x"))
  , testFileFromString 4 toProgram "(define x 1) x" $
      Right (Program [Def "x" (Num 1)] (Var "x"))
  , testFileFromString 5 toProgram "(define x 1)" $ Left "Invalid expression (define x 1)"
  ]
