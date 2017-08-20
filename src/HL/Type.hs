module HL.Type where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data PolyType = Forall [String] Type deriving (Eq, Show)

data Type = TApp Type Type
          | TLeaf String
          deriving (Eq, Show)

data Kind = Concrete | KApp Kind Kind | KFree | KVar String deriving (Eq, Show)

infixr 1 ~>
(~>) :: Kind -> Kind -> Kind
a ~> b = KApp a b

o :: Kind
o = Concrete

builtinKinds :: Map String Kind
builtinKinds = Map.fromList [ ("Bool", o)
                            , ("List", o ~> o)
                            , ("Num", o)
                            , ("->", o ~> o ~> o)
                            -- , ("Unit", o)
                            ]

fixArrowTypes :: Type -> Type
fixArrowTypes t@TLeaf{} = t
fixArrowTypes (TApp f@(TLeaf "->") x) = TApp f (fixArrowTypes x)
fixArrowTypes (TApp f x)
  | isArrow f = foldl combine (fixArrowTypes x) paired
  | otherwise = TApp (fixArrowTypes f) (fixArrowTypes x)
  where
    isArrow (TLeaf name) = name == "->"
    isArrow (TApp f' _) = isArrow f'
    revFlatten l@TLeaf{} = [l]
    revFlatten (TApp f' x') = x' : revFlatten f'
    paired = map (TApp (TLeaf "->")) . init . revFlatten $ f
    combine = flip TApp
