{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- |
--
-- Desugar.AExpr defines how to desugar While+ arithmetic expressions.
--

module Desugar.AExpr (desugarAExpr) where

import Lang
import qualified Desugar.DesugaredLang as DL

desugarAExpr :: AExpr -> DL.AExpr
desugarAExpr (NumLit n)           = DL.NumLit n
desugarAExpr (AVar name)          = DL.AVar name
desugarAExpr (ArithBinOp op a a') = desugarArithBinOp op (desugarAExpr a) $ desugarAExpr a'


desugarArithBinOp :: ArithBinOp -> DL.AExpr -> DL.AExpr ->
                     DL.AExpr
desugarArithBinOp OpSum = DL.ArithBinOp DL.OpSum
desugarArithBinOp OpMul = DL.ArithBinOp DL.OpMul
desugarArithBinOp OpSub = DL.ArithBinOp DL.OpSub
