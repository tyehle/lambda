{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HL.Environment
  ( Env, EnvT
  , evalEnv, evalEnvT
  , get, set
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Identity (Identity, runIdentity)

newtype EnvT k v m a = EnvT (StateT (Map k v) m a) deriving (Functor, Applicative, Monad, MonadTrans)

evalEnvT :: (Monad m) => EnvT k v m a -> Map k v -> m a
evalEnvT (EnvT state) = evalStateT state

type Env k v = EnvT k v Identity

evalEnv :: Env k v a -> Map k v -> a
evalEnv env initial = runIdentity $ evalEnvT env initial

get :: (Ord k, Monad m) => k -> EnvT k v m (Maybe v)
get = EnvT . gets . Map.lookup

set :: (Ord k, Monad m) => k -> v -> EnvT k v m ()
set key = EnvT . modify . Map.insert key
