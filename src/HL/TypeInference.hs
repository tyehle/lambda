{-# LANGUAGE FlexibleContexts #-}

module HL.TypeInference where

import HL.Type
import HL.Typed
import HL.Environment (EnvT, evalEnvT)
import HL.Fresh (Fresh, defaultEvalFresh)
import HL.MonadClasses (freshFrom, get, set)

import Data.Map (Map)
import Control.Monad.Trans.Except
import Control.Monad (when)

-- infer :: Exp -> Maybe Type
-- infer (Var x) = undefined x
-- infer (Num _) = Just TNum
-- infer (Lambda args body) = TFun <$> undefined args <*> infer body
-- infer (Let bindings body) = undefined bindings >> infer body
-- infer (Letrec name binding body) = undefined name binding >> infer body
-- infer (Application f x) = undefined f x

type Infer s = ExceptT String (EnvT String s Fresh)

runInfer :: Map String s -> Infer s a -> Either String a
runInfer env = defaultEvalFresh . flip evalEnvT env . runExceptT

inferKind :: PolyType -> Either String Kind
inferKind (Forall tVars kType) = runInfer builtinKinds $ do
  -- Add the type variables as fresh vars to the environment
  mapM_ (`set` KFree) tVars
  inferMonoKind kType >>= resolve

inferMonoKind :: Type -> Infer Kind Kind
inferMonoKind (TLeaf name) = return $ KVar name
inferMonoKind (TApp f a) = do
  freshName <- freshFrom "resultKind"
  set freshName KFree
  let resultKind = KVar freshName
  ak <- inferMonoKind a
  fk <- inferMonoKind f
  unify (KApp ak resultKind) fk
  return resultKind

resolve :: Kind -> Infer Kind Kind
resolve (KVar name) = lookupEnv name >>= resolve
resolve otherKind = return otherKind

unify :: Kind -> Kind -> Infer Kind ()
unify ka@(KVar aName) kb = do
  aBinding <- lookupEnv aName
  case aBinding of
    KFree -> do
      kb' <- resolve kb
      when (kb' /= ka) $ do
        occurs <- ka `occursIn` kb'
        if occurs
          then throwE $ "Cannot build infine kind " ++ show ka ++ " := " ++ show kb'
          else set aName kb'
    ka' -> unify ka' kb
unify ka kb@KVar{} = unify kb ka
unify Concrete Concrete = return ()
unify (KApp af ax) (KApp bf bx) = unify af bf >> unify ax bx
unify a b = throwE $ "Cannot unify " ++ show a ++ " with " ++ show b

occursIn :: Kind -> Kind -> Infer Kind Bool
_ `occursIn` KFree = return False
_ `occursIn` Concrete = return False
ka `occursIn` (KApp bf bx) = do
  inF <- ka `occursIn` bf
  inX <- ka `occursIn` bx
  return $ inF || inX
ka `occursIn` kb@(KVar bName) | ka == kb = return True
                              | otherwise = do
  bound <- lookupEnv bName
  ka `occursIn` bound

lookupEnv :: String -> Infer a a
lookupEnv name = get name >>= maybe (throwE ("Unbound variable: " ++ show name)) return

test :: Either String Kind
test = inferKind (Forall ["a"] (TApp (TLeaf "List") (TLeaf "a")))


checkExp :: Exp -> Infer Type PolyType
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
