{-# LANGUAGE UnicodeSyntax #-}

-- |
--
-- FixPoint defines semantic utility function to apply the Knaster-Taski-Kleene
-- fixed-point theorem
--
-- Partial functions from a state to another state are modeled via the 'Partial' type:
-- > type Partial state = state -> Maybe state
--
-- The type of the functional F is represented as a function between partial functions on states:
-- > type F state = Partial state -> Partial state
--
-- The type of the Fix function takes a functional F, a state, and returns a new state:
-- > type Fix state = F state -> state -> state
--
module FixPoint (
    cond
  , id'
  , fix) where

type Partial state = state -> Maybe state
type F state = Partial state -> Partial state
type Fix state = F state -> state -> state


-- | Conditional utility for evaluating functionals
cond :: (state -> Bool, Partial state, Partial state)
        -> Partial state
cond (bExpr, g, g') state = if bExpr state
  then g state
  else g' state


-- | Utility for mapping the id semantics in partial functions between states.
-- We compose 'id :: state -> state' with 'Just :: state -> Maybe state' 
id' :: Partial state
id' = Just


-- | Least element ⊥ of the (state -> Maybe state, ⊑) ccpo
bottom :: Partial state
bottom = const Nothing


-- Knaster-Tarski-Kleene fixed-point theorem application (Theorem 4.37)
fix :: Fix state
fix f = lub [f'n f n bottom | n <- [0..]]


-- | Power of functional f (Theorem 4.37)
f'n :: F state         -- ^ functional
      -> Int           -- ^ power of the functional
      -> Partial state -- ^ input of the functional
      -> Partial state -- ^ n-th application of functional
f'n _ 0 = id
f'n f n = f . f'n f (n - 1)


-- | Least-upper bound ⊔ of a chain Y (Lemma 4.25)
lub :: [Partial state] -> state -> state
lub (g:gs) s = case g s of
  Nothing   -> lub gs s
  (Just s') -> s'


-- Semantically equivalent definition of lub that preserves
-- tail-recursion.
-- lub' :: [Partial state] -> state -> state
-- lub' (g:gs) s = fromMaybe (lub' gs s) $ g s
