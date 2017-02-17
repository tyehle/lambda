module LazyInterpreter where

import Data.STRef
import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans

import Data.Map (Map)
import qualified Data.Map as Map

import Node
import Scope


data Box s = Forced (Value s) | Thunk Node (Env s)

data Value s = Clos String Node (Env s)

type Env s = Map String (STRef s (Box s))


interp :: Node -> ExceptT String (ST s) (Value s)
interp = lazyInterp Map.empty

lazyInterp :: Env s -> Node -> ExceptT String (ST s) (Value s)
lazyInterp env (Lam arg body) = return $ Clos arg body env
lazyInterp env (Ref x) = maybe (err x) force (Map.lookup x env)
  where
    err = ExceptT . return . scopeError
lazyInterp env (App f x) = do
  (Clos arg body env') <- lazyInterp env f
  argThunk <- lift $ newSTRef (Thunk x env)
  let extendedEnv = Map.insert arg argThunk env'
  lazyInterp extendedEnv body

force :: STRef s (Box s) -> ExceptT String (ST s) (Value s)
force ref = lift (readSTRef ref) >>= handleBox
  where
    handleBox (Forced v) = return v
    handleBox (Thunk node env) = do
      value <- lazyInterp env node
      lift $ writeSTRef ref (Forced value)
      return value
