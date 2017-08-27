module HL.TypeInferenceSpec (typeInferenceTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad ((>=>))

import HL.SExp (SExp)
import HL.MonadClasses (set)
import HL.Type
import HL.TypeInference
import qualified HL.Typed as Typed


typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ fixArrowTests
  , kindInferenceTests
  , unifyTests
  -- , literalTests
  ]


parseSExp :: String -> Either String SExp
parseSExp input = head <$> Typed.fromFile "input" input


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
  where
    testArrow :: Int -> String -> String -> TestTree
    testArrow n typeStr expectedStr = testCase (show n) (fixed @?= expected)
      where
        fixed = fixArrowTypes <$> (parseSExp typeStr >>= Typed.toType)
        expected = parseSExp expectedStr >>= Typed.toType


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
    , testKindInf 5 "(forall a (-> a))" $ Concrete ~> Concrete
    , testKindInf 6 "(forall (f a) (-> a (f a)))" Concrete
    , testKindInf 7 "(forall (m a b) (-> (m a) (-> a (m b)) (m b)))" Concrete
    , testKindInf 8 "(forall a (a (a Num Num)))" $ Concrete ~> Concrete
    , testCase (show (9::Int)) $ infer "(forall a (a a))" @?=
      Left "Cannot build infinite kind KVar \"a\" := KApp (KVar \"a\") (KVar \"_k_0\")"
    ]
  ]
  where
    infer :: String -> Either String Kind
    infer = parseSExp >=> Typed.toPolyType >=> inferKind
    testKindInf :: Int -> String -> Kind -> TestTree
    testKindInf n typeStr kind = testCase (show n) (infer typeStr @?= Right kind)


testKindInfer :: (Eq a, Show a) => Int -> Either String a -> Infer Kind a -> TestTree
testKindInfer n expected action = testCase (show n) assertion
  where
    assertion = runInfer builtinKinds action @?= expected

unifyTests :: TestTree
unifyTests = testGroup "Kind Unification Tests"
  [ testKindInfer 1 (Right Concrete) $ do
      set "a" KFree
      unify (KVar "a") Concrete
      resolve (KVar "a")
  , testKindInfer 2 (Right Concrete) $ do
      set "a" KFree; set "b" KFree
      unify (KVar "a") (KVar "b")
      unify (KVar "a") Concrete
      resolve (KVar "b")
  , testKindInfer 3 (Right Concrete) $ do
      set "a" KFree; set "b" KFree
      unify (KVar "b") (KVar "a")
      unify (KVar "a") Concrete
      resolve (KVar "b")
  , testKindInfer 4 (Right ()) $ unify Concrete Concrete
  , testKindInfer 5 (Right (Concrete, KVar "steakSauce")) $ do
      set "x" KFree; set "y" KFree
      set "a1" Concrete; set "steakSauce" KFree
      unify (KApp (KVar "x") (KVar "y")) (KApp (KVar "a1") (KVar "steakSauce"))
      x <- lookupEnv "x"
      y <- lookupEnv "y"
      return (x, y)
  , testKindInfer 6 (Left "Cannot unify KFree with Concrete") (unify KFree Concrete)
  ]
