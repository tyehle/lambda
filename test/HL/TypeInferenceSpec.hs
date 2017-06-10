module HL.TypeInferenceSpec (typeInferenceTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec (parse)

import HL.TypeInference
import qualified HL.Typed as Typed
import HL.SExp

typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ literalTests
  ]

testExp :: Int -> Typed.Exp -> KType -> TestTree
testExp n expr expected = testCase (show n) assertion
  where
    assertion = runChecker (checkExp expr) @?= Right expected

parseType :: String -> Typed.Type
parseType = either (error . show) id . parse sExp "Test result"

literalTests :: TestTree
literalTests = testGroup "Literal Tests"
  [ testExp 1 (Typed.Num 1) num
  , testExp 2 (Typed.Lambda ["x"] (Typed.Var "x"))
      (KType (Typed.Forall ["a"] (parseType "(-> a a)")) K)
  ]
