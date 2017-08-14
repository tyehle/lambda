module HL.Type where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data PolyType = Forall [String] KType deriving (Eq, Show)

data KType = TApp KType KType
           | TLeaf Kind String
           deriving (Eq, Show)

data Kind = Concrete | KApp Kind Kind | KUnknown deriving (Eq, Show)

infixl 1 ~>
(~>) :: Kind -> Kind -> Kind
a ~> b = KApp a b

o :: Kind
o = Concrete

k :: Kind
k = KUnknown

builtinKinds :: Map String Kind
builtinKinds = Map.fromList [ ("Bool", o)
                            , ("List", o ~> o)
                            , ("Num", o)
                            , ("->", o ~> o ~> o)
                            -- , ("Unit", o)
                            ]
