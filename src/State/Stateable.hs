{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- 
-- State.Stateable provides an abstract representation of the computation state.
-- The concrete state will have to implement the interface defined in
-- the 'Stateable k v' typeclass.
--
-- > data S k v :: *
--
-- The state data will be contained in a key-value map 'S' where the keys 'k'
-- represent variable names and the values 'v' represent the corresponding
-- variable values.

module State.Stateable (Stateable (..)) where


class Stateable k v where
  data S k v :: *

  -- | Return a new empty state
  empty :: S k v

  -- | Return True iff the state is empty
  isEmpty :: S k v -> Bool

  -- | Return a variable from the state. This variable must be defined
  lookupVar :: k -> S k v -> v

  -- | Write a new variable in the state or update a previously defined variable
  insertVar :: k -> v -> S k v -> S k v

  -- | Return the list of variables used in the state
  getVars :: S k v -> [k]
