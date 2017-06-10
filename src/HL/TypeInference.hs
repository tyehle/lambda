module HL.TypeInference where

import HL.Typed
import HL.SExp (SExp(..))
import HL.Fresh

import Control.Monad.Trans.Except

-- infer :: Exp -> Maybe Type
-- infer (Var x) = undefined x
-- infer (Num _) = Just TNum
-- infer (Lambda args body) = TFun <$> undefined args <*> infer body
-- infer (Let bindings body) = undefined bindings >> infer body
-- infer (Letrec name binding body) = undefined name binding >> infer body
-- infer (Application f x) = undefined f x

type Check a = ExceptT String Fresh a

runChecker :: Check a -> Either String a
runChecker = defaultEvalFresh . runExceptT

data Kind = K | KApp Kind Kind deriving (Eq, Show)

infixl 1 -->
(-->) :: Kind -> Kind -> Kind
a --> b = KApp a b

data KType = KType QType Kind deriving (Eq, Show)
-- data Type = Type String Kind deriving (Eq, Show)

checkExp :: Exp -> Check KType
checkExp (Var _) = undefined
checkExp (Num _) = return num
checkExp (EAnn e t) = checkExp e >>= undefined t
checkExp (Lambda args body) = undefined args >> checkExp body
checkExp (Let bindings body) = undefined bindings >> checkExp body
checkExp (Letrec name value body) = undefined name value >> checkExp body
-- checkExp (Typed.Case expr clauses) = checkExp expr >>= undefined clauses
checkExp (Application f x) = do
  fType <- checkExp f
  xType <- checkExp x
  return $ undefined fType xType

monoType :: String -> Kind -> KType
monoType name = KType (Forall [] (Leaf name))

bool :: KType
bool = monoType "Bool" K

list :: KType
list = monoType "List" (K --> K)

num :: KType
num = monoType "Num" K

function :: KType
function = monoType "->" (K --> K --> K)
