{-# LANGUAGE Rank2Types #-}

module LazyInterpreter where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.STRef
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.ST

import Node
import Scope

data Box s = Forced (Result s) | Thunk Node (Env s)

data Result s = Clos String Node (Env s)
              | RFun (Result s -> ExceptT String (ST s) (Result s))
              | RNum Integer
              | RBool Bool
              | RPair (Result s, Result s)
              | REmpty

type Env s = Map String (STRef s (Box s))

type EST s a = ExceptT String (ST s) a

lazyInterp :: Env s -> Node -> EST s (Result s)
lazyInterp env (Lam arg body) = return $ Clos arg body env
lazyInterp env (Ref x) = maybe (scopeExcept x) force (Map.lookup x env)
lazyInterp env (App f x) = lazyInterp env f >>= (`app` Thunk x env)

force :: STRef s (Box s) -> EST s (Result s)
force ref = lift (readSTRef ref) >>= handleBox
  where
    handleBox (Forced r) = return r
    handleBox (Thunk node env) = do
      result <- lazyInterp env node
      lift $ writeSTRef ref (Forced result)
      return result

app :: Result s -> Box s -> EST s (Result s)
app (RFun f) (Forced x) = f x
app (RFun f) (Thunk node env) = lazyInterp env node >>= f
app (Clos arg body env) box = do
  argRef <- lift $ newSTRef box
  let extendedEnv = Map.insert arg argRef env
  lazyInterp extendedEnv body
app _ _ = throwE "Cannot apply non-function type"

unwrap :: Result s -> EST s Node
unwrap (Clos arg body env) = bindAll toBind $ Lam arg body
  where
    toBind = Set.toList $ arg `Set.delete` freeVars body
    bindAll [] expr = return expr
    bindAll (x:xs) expr = do
      binding <- maybe (scopeExcept x) force $ Map.lookup x env
      rawBinding <- unwrap binding
      bindAll xs $ Lam x expr `App` rawBinding
unwrap _ = throwE "Cannot unwrap non-closure type"

scopeExcept :: String -> EST s a
scopeExcept = ExceptT . return . scopeError


interp :: Node -> Either String Node
interp input = runST $ runExceptT (lazyInterp Map.empty input >>= unwrap)


extract :: (forall s. Result s -> EST s a) -> Node -> Either String a
extract ex input = runST $ runExceptT (lazyInterp Map.empty input >>= ex)


extractInt :: Node -> Either String Integer
extractInt = extract intExtractor

extractBool :: Node -> Either String Bool
extractBool = extract boolExtractor

extractList :: (forall s. Result s -> EST s a) -> Node -> Either String [a]
extractList ex = extract $ listExtractor ex


intExtractor :: Result s -> EST s Integer
intExtractor res = res `app` Forced (RFun plus1)
               >>= (`app` Forced (RNum 0))
               >>= getNum
  where
    plus1 (RNum n) = return $ RNum (n+1)
    plus1 _ = throwE "Cannot increment non-number type"
    getNum (RNum n) = return n
    getNum _ = throwE "Non-number result"

boolExtractor :: Result s -> EST s Bool
boolExtractor res = res `app` Forced (RBool True)
                >>= (`app` Forced (RBool False))
                >>= getBool
  where
    getBool (RBool b) = return b
    getBool _ = throwE "Non-boolean result"

listExtractor :: (Result s -> EST s a) -> Result s -> EST s [a]
listExtractor ex res = res `app` Forced (RFun onCons)
                   >>= (`app` Forced (RFun onEmpty))
                   >>= pairToList
  where
    onCons hd = return $ RFun (\tl -> return (RPair (hd, tl)))
    onEmpty _ = return REmpty
    pairToList (RPair (hd,tl)) = do
      x <- ex hd
      xs <- listExtractor ex tl
      return (x:xs)
    pairToList REmpty = return []
    pairToList _ = throwE "Non-list result"
