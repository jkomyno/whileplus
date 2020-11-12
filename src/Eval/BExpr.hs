{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Eval.BExpr (evalBExpr) where

import Desugar.DesugaredLang
import Eval.AExpr
import State (State)

evalBExpr :: BExpr -> State -> Bool
evalBExpr (BoolLit b)              _     = b
evalBExpr (BoolUnOp op b)          state = evalBoolUnOp op b state
evalBExpr (BoolBinOp op b1 b2)     state = evalBoolBinOp op b1 b2 state
evalBExpr (ArithRelation op a1 a2) state = evalArithRelation op a1 a2 state


evalBoolUnOp :: BoolUnOp -> BExpr -> State -> Bool
evalBoolUnOp op b state = let
  t = evalBExpr b state
  in evalBoolUnOp' op t


evalBoolUnOp' :: BoolUnOp -> Bool -> Bool
evalBoolUnOp' OpNeg = not


evalBoolBinOp :: BoolBinOp -> BExpr -> BExpr -> State -> Bool
evalBoolBinOp op b b' state = let
  t  = evalBExpr b  state
  t' = evalBExpr b' state
  in evalBoolBinOp' op t t'

evalBoolBinOp' :: BoolBinOp -> Bool -> Bool -> Bool
evalBoolBinOp' OpAnd = (&&)


evalArithRelation :: ArithRelation -> AExpr -> AExpr -> State -> Bool
evalArithRelation op a a' state = let
  n  = evalAExpr a  state
  n' = evalAExpr a' state
  in evalArithRelation' op n n'


evalArithRelation' :: Ord a => ArithRelation -> a -> a -> Bool
evalArithRelation' OpEq  = (==)
evalArithRelation' OpLte = (<=)
