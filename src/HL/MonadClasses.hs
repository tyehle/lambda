{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module HL.MonadClasses where

import HL.Fresh (FreshT)
import qualified HL.Fresh as Fresh (freshFrom)
import HL.Environment (EnvT)
import qualified HL.Environment as Env (get, set)

import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT)


class Monad m => MonadFresh m where
  freshFrom :: String -> m String

instance Monad m => MonadFresh (FreshT m) where
  freshFrom = Fresh.freshFrom

instance MonadFresh m => MonadFresh (ExceptT e m) where
  freshFrom = lift . freshFrom

instance MonadFresh m => MonadFresh (EnvT k v m) where
  freshFrom = lift . freshFrom


class Monad m => MonadEnv k v m where
  get :: Ord k => k -> m (Maybe v)
  set :: Ord k => k -> v -> m ()

instance Monad m => MonadEnv k v (EnvT k v m) where
  get = Env.get
  set = Env.set

instance MonadEnv k v m => MonadEnv k v (ExceptT e m) where
  get = lift . get
  set key value = lift $ set key value

instance MonadEnv k v m => MonadEnv k v (FreshT m) where
  get = lift . get
  set key value = lift $ set key value
