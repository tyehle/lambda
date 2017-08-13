module HL.TypeInferenceSpec (typeInferenceTests) where

import Test.Tasty
import Test.Tasty.HUnit

import HL.Type
import HL.TypeInference
import qualified HL.Typed as Typed

typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ literalTests
  ]

testExp :: Int -> Typed.Exp -> PolyType -> TestTree
testExp n expr expected = testCase (show n) assertion
  where
    assertion = runChecker (checkExp expr) @?= Right expected

literalTests :: TestTree
literalTests = testGroup "Literal Tests"
  [ testExp 1 (Typed.Num 1) num
  , testExp 2 (Typed.Lambda ["x"] (Typed.Var "x"))
      (Forall ["x"] (TApp k (TApp k (TLeaf k "->") (TLeaf k "x"))
                            (TLeaf k "x")))
  ]
