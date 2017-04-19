{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HL.Fresh
  ( Fresh
  , runFresh, evalFresh, execFresh, defaultEvalFresh
  , FreshT
  , runFreshT, evalFreshT, defaultEvalFreshT, execFreshT
  , freshFrom
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe (fromMaybe)


type Used = Map String Int

newtype FreshT m a = FreshT (StateT Used m a) deriving (Functor, Applicative, Monad, MonadTrans)

runFreshT :: FreshT m a -> Used -> m (a, Used)
runFreshT (FreshT s) = runStateT s

evalFreshT :: Functor m => FreshT m a -> Used -> m a
evalFreshT f u = fst <$> runFreshT f u

defaultEvalFreshT :: Functor m => FreshT m a -> m a
defaultEvalFreshT f = evalFreshT f Map.empty

execFreshT :: Functor m => FreshT m a -> Used -> m Used
execFreshT f u = snd <$> runFreshT f u

freshFrom :: Monad m => String -> FreshT m String
freshFrom base = FreshT $ do
  n <- gets $ fromMaybe 0 . Map.lookup base
  modify $ Map.insert base (n+1)
  return $ "_" ++ base ++ "_" ++ show n


type Fresh = FreshT Identity

runFresh :: Fresh a -> Used -> (a, Used)
runFresh f u = runIdentity $ runFreshT f u

evalFresh :: Fresh a -> Used -> a
evalFresh f = fst . runFresh f

defaultEvalFresh :: Fresh a -> a
defaultEvalFresh f = evalFresh f Map.empty

execFresh :: Fresh a -> Used -> Used
execFresh f = snd . runFresh f
