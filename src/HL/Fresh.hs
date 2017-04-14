{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HL.Fresh (Fresh, runFresh, evalFresh, execFresh, defaultEvalFresh, freshFrom) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)

newtype Fresh a = Fresh (State Used a) deriving (Functor, Applicative, Monad)

type Used = Map String Int

runFresh :: Fresh a -> Used -> (a, Used)
runFresh (Fresh s) = runState s

evalFresh :: Fresh a -> Used -> a
evalFresh f = fst . runFresh f

defaultEvalFresh :: Fresh a -> a
defaultEvalFresh f = evalFresh f Map.empty

execFresh :: Fresh a -> Used -> Used
execFresh f = snd . runFresh f

freshFrom :: String -> Fresh String
freshFrom base = Fresh $ do
  n <- gets $ fromMaybe 0 . Map.lookup base
  modify $ Map.insert base (n+1)
  return $ "_" ++ base ++ "_" ++ show n
