{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- Concrete implementation of the state, which is represented as a Map
-- between variable names and the corresponding numeric values.

module State (
    Stateable (..)
  , State (..)
) where

import State.Stateable (Stateable (..))
import Lang (Name)

import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe (fromJust)

-- | A State is a container S that assigns each variable name to its numeric value
type State = S Name Integer


instance (Ord k, Show k, Show v) => Stateable k v where
  data S k v = S (Map.Map k v)
  
  -- | Return a new empty state
  -- empty :: S k v
  empty = S Map.empty

  -- | Return True iff the state is empty
  -- isEmpty :: S k v -> Bool
  isEmpty (S s) = Map.null s

  -- | Return a variable from the state. This variable must be defined
  -- lookupVar :: k -> S k v -> v
  lookupVar k (S s) = fromJust $ Map.lookup k s

  -- | Write a new variable in the state or update a previously defined variable
  -- insertVar :: k -> v -> S k v -> S k v
  insertVar k v (S s) = S (Map.insert k v s)

  -- | Return the list of variables used in the state
  -- getVars :: S k v -> [k]
  getVars (S s) = fst <$> Map.toAscList s


instance (Ord k, Show k, Show v) => Show (S k v) where
  -- | User-friendly key-value map representation
  -- show :: State -> String
  show state@(S s)
    | isEmpty state = "State is empty"
    | otherwise     = intercalate "\n"
      ((\(k, v) -> concat ["  ", show k, " -> ", show v]) <$> Map.toList s)
