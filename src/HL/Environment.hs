{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HL.Environment
  ( Env
  , evalEnv
  , get, set
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict (State, evalState, gets, modify)

newtype Env k v a = Env (State (Map k v) a) deriving (Functor, Applicative, Monad)

evalEnv :: Env k v a -> Map k v -> a
evalEnv (Env e) = evalState e

get :: (Ord k) => k -> Env k v (Maybe v)
get = Env . gets . Map.lookup

set :: (Ord k) => k -> v -> Env k v ()
set key = Env . modify . Map.insert key
