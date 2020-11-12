-- |
--
-- Check.CheckVariablesWithState is responsible of traversing a desugared AST to ensure that the variable
-- declarations and assignments are compatible with an error-free runtime execution.
-- CheckVariablesWithState receives the initial.
-- If one assignment relies on a variable that has been referenced before being defined,
-- the check fails and the UnboundVarError is returned.
-- The traversal happens top-down, and it is a purely syntactical step.
--

module Check.CheckVariablesWithState (checkVariablesWithState) where

import qualified Data.Set as Set
import Data.Bifunctor (first)

import Check.CheckVariables (
    checkVariables'
  , Store)
import Desugar.DesugaredLang
import qualified LangException as LE
import State (
    State
  , getVars)

initialStore :: State -> Store
initialStore state = Set.fromAscList $ getVars state

-- | Traverse the AST from the top-down, returning true if every variable
-- is bounded. This analysis is purely syntactical, no semantic
-- execution is run. A set is used as store for writing the variable names
-- met during the AST traversal.
checkVariablesWithState :: State -> DesugaredProgram -> Either LE.LangException ()
checkVariablesWithState _     (DesugaredProgram [])     = Right ()
checkVariablesWithState state (DesugaredProgram (x:xs)) = let
  store  = initialStore state
  result = checkVariables' store (x:xs)
  in first (LE.UnboundVarError . show) result
