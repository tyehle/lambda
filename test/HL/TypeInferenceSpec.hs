module HL.TypeInferenceSpec (typeInferenceTests) where

import Test.Tasty
import Test.Tasty.HUnit

import HL.Type
import HL.TypeInference
import qualified HL.Typed as Typed

import qualified Data.Map as Map
import Control.Monad ((>=>))

typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ kindInferenceTests
  -- , literalTests
  ]

testExp :: Int -> Typed.Exp -> PolyType -> TestTree
testExp n expr expected = testCase (show n) assertion
  where
    assertion = runInfer Map.empty (checkExp expr) @?= Right expected

literalTests :: TestTree
literalTests = testGroup "Literal Tests"
  [ testExp 1 (Typed.Num 1) (Forall [] (TLeaf "Num"))
  , testExp 2 (Typed.Lambda ["x"] (Typed.Var "x"))
      (Forall ["x"] (TApp (TApp (TLeaf "->") (TLeaf "x"))
                            (TLeaf "x")))
  ]

parsePolyType :: String -> Either String PolyType
parsePolyType = parse >=> Typed.toPolyType
  where
    parse s = head <$> Typed.fromFile "input" s

testKindInf :: Int -> String -> Kind -> TestTree
testKindInf n typeStr kind = testCase (show n) assertion
  where
    parsed = parsePolyType typeStr
    assertion = (parsed >>= inferKind) @?= Right kind

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
