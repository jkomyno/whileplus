{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- |
--
-- Eval.Evaluator exposes an evaluation function that relies on the Knaster-Tarski-Kleeni
-- iteration named 'eval'. It takes a desugared AST of the original While+ source program,
-- and initial state and it evaluates every statement sequentially to return the final state.
--
-- > eval :: DesugaredProgram -> State -> State
--
module Eval.Evaluator (eval) where

import Data.Foldable (foldl')

import Desugar.DesugaredLang
import State (
    State
  , insertVar)
import Eval.AExpr (evalAExpr)
import Eval.BExpr (evalBExpr)
import FixPoint (
    cond
  , id'
  , fix)

-- | Evaluate the entire desugared program and return the final state
eval :: DesugaredProgram -> State -> State
eval (DesugaredProgram xs) state = foldl' (flip evalStmt) state xs


-- Evaluate the given statement and return the updated state
evalStmt :: Statement -> State -> State
evalStmt (Assignment x a)     = evalAssignment x a
evalStmt (PairAssignment n a) = evalPairAssignment n a
evalStmt Skip                 = id
evalStmt (Conditional b t f)  = evalConditional b t f
evalStmt (While b stmt)       = evalWhile b stmt
evalStmt (Repeat' stmt b)     = evalRepeat' stmt b
evalStmt (Composition ss)     = evalComposition ss


-- | Store a new variable declaration into the environment.
-- If a variable with the same name already exists, it overrides that.
evalAssignment :: Name -> AExpr ->
                  State -> State
evalAssignment x a state = let
  a'     = evalAExpr a state
  state' = insertVar x a' state
  in state' 


evalPairAssignment :: (Name, Name) -> (AExpr, AExpr) -> State -> State
evalPairAssignment (name, name') (a, a') state = let
  n = evalAExpr a state
  m = evalAExpr a' state
  state'  = insertVar name n state
  state'' = insertVar name' m state'
  in state''

evalConditional :: BExpr -> Statement -> Statement ->
                   State -> State
evalConditional b t f state =
  if evalBExpr b state
    then evalStmt t state
    else evalStmt f state


evalWhile :: BExpr -> Statement ->
             State -> State
evalWhile b stmt = let
  f g = cond (evalBExpr b, g . evalStmt stmt, id')
  in fix f


evalRepeat' :: Statement -> BExpr ->
               State -> State
evalRepeat' stmt b = let
  f' g = cond (evalBExpr b, id', g) . evalStmt stmt
  in fix f'


evalComposition :: [Statement] ->
                   State -> State
evalComposition = flip (foldl' (flip evalStmt))
