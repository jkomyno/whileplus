-- |
--
-- Check.CheckVariables is responsible of traversing a desugared AST to ensure that the variable
-- declarations and assignments are compatible with an error-free runtime execution.
-- CheckVariables assumes that the initial state of the computation is empty.
-- If one assignment relies on a variable that has been referenced before being defined,
-- the check fails and the UnboundVarError is returned.
-- The traversal happens top-down, and it is a purely syntactical step.
--


module Check.CheckVariables (
    Store
  , checkVariables
  , checkVariables') where

import qualified Data.Set as Set
import Data.Bifunctor (first)

import Desugar.DesugaredLang
import qualified LangException as LE

type Store = Set.Set Name


-- | Traverse the AST from the top-down, returning true if every variable
-- is bounded. This analysis is purely syntactical, no semantic
-- execution is run. A set is used as store for writing the variable names
-- met during the AST traversal.
checkVariables :: DesugaredProgram -> Either LE.LangException ()
checkVariables (DesugaredProgram [])     = Right ()
checkVariables (DesugaredProgram (x:xs)) = let
  result = checkVariables' Set.empty (x:xs)
  in first (LE.UnboundVarError . show) result


checkVariables' :: Store -> [Statement] -> Either Name ()
checkVariables' _ []         = Right ()
checkVariables' store (x:xs) = do
  store' <- checkVarsInStatement store x
  checkVariables' store' xs


-- | Check that every variable in a statement is bounded. Since some
-- statements can update the state, the result contains both a boolean with
-- the partial answer and a possibly updated store.
checkVarsInStatement :: Store -> Statement -> Either Name Store

-- An assignment is valid if the variable referenced in the arithmetical
-- expression has already been written to the store. We also need to
-- write the possibly new variable @name@ to the store.
checkVarsInStatement store (Assignment name a) = do
  checkVarsInAExpr store a
  let store' = Set.insert name store
  Right store'

checkVarsInStatement store (Composition [])     = return store
checkVarsInStatement store (Composition (s:ss)) = do
  store' <- checkVarsInStatement store s
  checkVarsInStatement store' (Composition ss) 

-- Skip: trivially checked
checkVarsInStatement store Skip = Right store

-- While: we propagate the check inside the statement
checkVarsInStatement store (While b s) = checkVarsInBExpr store b >> checkVarsInStatement store s

-- Repeat': we propagate the check inside the statement
checkVarsInStatement store (Repeat' s b) = checkVarsInBExpr store b >> checkVarsInStatement store s

-- Conditional: we propagate the check inside the 2 branched statements
checkVarsInStatement store (Conditional b s1 s2) = do
  checkVarsInBExpr store b
  store'  <- checkVarsInStatement store  s1
  store'' <- checkVarsInStatement store' s2
  Right store''


-- | Check that every variable in boolean expressions is bounded
checkVarsInBExpr :: Store -> BExpr -> Either Name ()

-- Boolean literal: trivially checked
checkVarsInBExpr _ (BoolLit _) = Right ()

-- Boolean unary operator: we propagate the check inside the boolean expression
checkVarsInBExpr store (BoolUnOp _ b) = checkVarsInBExpr store b

-- Boolean binary operator: we propagate the check inside the 2 boolean expressions
checkVarsInBExpr store (BoolBinOp _ b b') = do
  checkVarsInBExpr store b
  checkVarsInBExpr store b'
  Right ()

-- Arithmetic binary relations with boolean results: we propagate the check inside
-- the 2 arithmetic expressions
checkVarsInBExpr store (ArithRelation _ a a') = do
  checkVarsInAExpr store a
  checkVarsInAExpr store a'
  Right ()


-- | Check that every variable in arithmetical expressions is bounded
checkVarsInAExpr :: Store -> AExpr -> Either Name ()

-- a variable is bounded if its name has already been written to the store
checkVarsInAExpr store (AVar x) = do
  if Set.member x store
    then Right ()
    else Left x

-- a numerical literal doesn't involve any variable, so it's trivially checked
checkVarsInAExpr _ (NumLit _) = return ()

-- recursively check both branches of a binary arithmetic operation
checkVarsInAExpr store (ArithBinOp _ a1 a2) = do
  checkVarsInAExpr store a1
  checkVarsInAExpr store a2
