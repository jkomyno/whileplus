{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Eval.AExpr (evalAExpr) where

import Desugar.DesugaredLang
import State (
    State
  , lookupVar)


evalAExpr :: AExpr -> State -> Integer
evalAExpr (NumLit n)                    _     = n
evalAExpr (AVar name)                   state = extractVar name state
evalAExpr (ArithBinOp arithBinOp a1 a2) state = evalArithBinOp arithBinOp a1 a2 state


-- Extracts the value of a variable.
-- Variable @x@ must exist in @state@.
extractVar :: Name -> State -> Integer
extractVar = lookupVar


evalArithBinOp :: ArithBinOp -> AExpr -> AExpr -> State -> Integer
evalArithBinOp op a a' state = let
  n  = evalAExpr a  state
  n' = evalAExpr a' state
  in evalArithBinOp' op n n'


evalArithBinOp' :: Num a => ArithBinOp -> a -> a -> a
evalArithBinOp' OpSum = (+)
evalArithBinOp' OpMul = (*)
evalArithBinOp' OpSub = (-)
