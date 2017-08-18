module HL.TypeInference where

import HL.Type
import HL.Typed
import HL.Environment

import qualified Data.Map as Map (empty)
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
import Control.Monad (when)

-- infer :: Exp -> Maybe Type
-- infer (Var x) = undefined x
-- infer (Num _) = Just TNum
-- infer (Lambda args body) = TFun <$> undefined args <*> infer body
-- infer (Let bindings body) = undefined bindings >> infer body
-- infer (Letrec name binding body) = undefined name binding >> infer body
-- infer (Application f x) = undefined f x

type Check a = ExceptT String (Env String PolyType) a

inferKind :: PolyType -> ExceptT String (Env String Kind) Kind
inferKind (Forall tVars kType) = do
  -- Add the type variables as fresh vars to the environment
  mapM_ (lift . flip set KFree) tVars
  inferMonoKind kType >>= resolve
  where
    inferMonoKind (TLeaf name) = lookupKind name
    inferMonoKind (TApp f a) = do
      freshName <- undefined
      let resultKind = KVar freshName
      lift $ set freshName KFree
      ak <- inferMonoKind a
      fk <- inferMonoKind f
      unify (KApp ak resultKind) fk
      return resultKind
    resolve (KVar name) = lookupKind name >>= resolve
    resolve otherKind = return otherKind
    unify ka@(KVar aName) kb = do
      aBinding <- lookupKind aName
      case aBinding of
        KFree -> do
          kb' <- resolve kb
          when (kb' /= ka) $
            if ka `occursIn` kb'
            then throwE $ "Cannot build infine kind " ++ show ka ++ " := " ++ show kb'
            else lift $ set aName kb'
        ka' -> unify ka' kb
    unify ka kb@KVar{} = unify kb ka
    unify Concrete Concrete = return ()
    unify (KApp af ax) (KApp bf bx) = unify af bf >> unify ax bx
    unify a b = throwE $ "Cannot unify " ++ show a ++ " with " ++ show b
    occursIn ka kb = undefined
    lookupKind name = do
      bound <- lift $ get name
      maybe (throwE ("Unbound type variable: " ++ show name)) return bound

test :: Either String Kind
test = flip evalEnv builtinKinds . runExceptT $ infered
  where
    infered = inferKind (Forall ["a"] (TApp (TLeaf "List") (TLeaf "a")))


runChecker :: Check a -> Either String a
runChecker = flip evalEnv Map.empty . runExceptT

checkExp :: Exp -> Check PolyType
checkExp (Var _) = undefined
checkExp (Num _) = return $ Forall [] $ TLeaf "Num"
checkExp (EAnn e t) = checkExp e >>= undefined t
checkExp (Lambda args body) = undefined args >> checkExp body
checkExp (Let bindings body) = undefined bindings >> checkExp body
checkExp (Letrec name value body) = undefined name value >> checkExp body
-- checkExp (Typed.Case expr clauses) = checkExp expr >>= undefined clauses
checkExp (Application f x) = do
  fType <- checkExp f
  xType <- checkExp x
  return $ undefined fType xType
