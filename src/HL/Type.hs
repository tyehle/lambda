module HL.Type where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data PolyType = Forall [String] Type deriving (Eq, Show)

data Type = TApp Type Type
          | TLeaf String
          deriving (Eq, Show)

data Kind = Concrete | KApp Kind Kind | KFree | KVar String deriving (Eq, Show)

infixl 1 ~>
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
