module HL.TypeInference where

import HL.Type
import HL.Typed
import HL.Environment

import Control.Monad.Trans.Except

-- infer :: Exp -> Maybe Type
-- infer (Var x) = undefined x
-- infer (Num _) = Just TNum
-- infer (Lambda args body) = TFun <$> undefined args <*> infer body
-- infer (Let bindings body) = undefined bindings >> infer body
-- infer (Letrec name binding body) = undefined name binding >> infer body
-- infer (Application f x) = undefined f x

type Check a = ExceptT String (Env String PolyType) a

runChecker :: Check a -> Either String a
runChecker = evalEnv . runExceptT

checkExp :: Exp -> Check PolyType
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
