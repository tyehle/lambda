module Scope where

import Data.Set (Set)
import qualified Data.Set as Set

class Scope a where
  freeVars :: a -> Set String

isFree :: Scope e => String -> e -> Bool
isFree name expr = name `Set.member` freeVars expr
