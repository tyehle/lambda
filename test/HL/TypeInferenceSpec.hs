module HL.TypeInferenceSpec (typeInferenceTests) where

import Test.Tasty
import Test.Tasty.HUnit

import HL.SExp (SExp)
import HL.Type
import HL.TypeInference
import qualified HL.Typed as Typed


typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ fixArrowTests
  , kindInferenceTests
  -- , literalTests
  ]

parseSExp :: String -> Either String SExp
parseSExp input = head <$> Typed.fromFile "input" input


testArrow :: Int -> String -> String -> TestTree
testArrow n typeStr expectedStr = testCase (show n) (fixed @?= expected)
  where
    fixed = fixArrowTypes <$> (parseSExp typeStr >>= Typed.toType)
    expected = parseSExp expectedStr >>= Typed.toType

fixArrowTests :: TestTree
fixArrowTests = testGroup "Fix Arrow Types"
  [ testArrow 1 "Num" "Num"
  , testArrow 2 "(List a)" "(List a)"
  , testArrow 3 "(-> a b)" "((-> a) b)"
  , testArrow 4 "(-> a b c)" "((-> a) ((-> b) c))"
  , testArrow 5 "(-> a)" "(-> a)"
  , testArrow 6 "(-> (List a))" "(-> (List a))"
  , testArrow 7 "(-> (-> a b))" "(-> ((-> a) b))"
  , testArrow 8 "(List (-> a b c))" "(List ((-> a) ((-> b) c)))"
  , testArrow 9 "(-> (-> a b) (-> c d))" "((-> ((-> a) b)) ((-> c) d))"
  ]


testKindInf :: Int -> String -> Kind -> TestTree
testKindInf n typeStr kind = testCase (show n) (infered @?= Right kind)
  where
    infered = parseSExp typeStr >>= Typed.toPolyType >>= inferKind

kindInferenceTests :: TestTree
kindInferenceTests = testGroup "Kind Inference"
  [ testGroup "Monomorphic Types"
    [ testKindInf 1 "(List Bool)" Concrete
    , testKindInf 2 "Num" Concrete
    , testKindInf 3 "List" $ Concrete ~> Concrete
    , testKindInf 4 "(-> Num)" $ Concrete ~> Concrete
    , testKindInf 5 "(-> Num Num)" Concrete
    , testKindInf 6 "(-> Bool Bool Bool)" Concrete
    ]
  , testGroup "Polymorphic Types"
    [ testKindInf 1 "(forall a (List a))" Concrete
    , testKindInf 2 "(forall a a)" KFree
    , testKindInf 3 "(forall (a b) (-> a b))" Concrete
    , testKindInf 4 "(forall (a b c) (-> a b c))" Concrete
    , testKindInf 5 "(forall (f a) (-> a (f a)))" Concrete
    , testKindInf 6 "(forall (m a b) (-> (m a) (-> a (m b)) (m b)))" Concrete
    , testKindInf 7 "(forall a (a (a Num Num)))" $ Concrete ~> Concrete
    ]
  ]
