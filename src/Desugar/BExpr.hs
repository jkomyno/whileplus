{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- |
--
-- Desugar.BExpr defines how to desugar While+ boolean expressions.
--

module Desugar.BExpr (
    desugarBExpr
  , desugarArithRelation) where

import Lang
import Desugar.AExpr (desugarAExpr)
import qualified Desugar.DesugaredLang as DL
import qualified Desugar.Utils as SU


-- | Desugar boolean expressions
desugarBExpr :: BExpr -> DL.BExpr
desugarBExpr (BoolLit l)             = DL.BoolLit l
desugarBExpr (BoolUnOp op b)         = desugarBoolUnOp op $ desugarBExpr b
desugarBExpr (BoolBinOp op b b')     = desugarBoolBinOp op (desugarBExpr b) $ desugarBExpr b'
desugarBExpr (ArithRelation op a a') = desugarArithRelation op (desugarAExpr a) $ desugarAExpr a'


-- | Desugar boolean unary operators
desugarBoolUnOp :: BoolUnOp -> DL.BExpr ->
                   DL.BExpr
desugarBoolUnOp OpNeg = DL.BoolUnOp DL.OpNeg


-- | Desugar boolean binary operators
desugarBoolBinOp :: BoolBinOp -> DL.BExpr -> DL.BExpr ->
                    DL.BExpr
desugarBoolBinOp OpAnd b b' = DL.BoolBinOp DL.OpAnd b b'

-- | Desugar the '||' (disjunction) boolean binary operator in terms of
-- negation and conjunction.
-- (b || b') => !(!b && !b')
desugarBoolBinOp OpOr  b b' = SU.negate $ DL.BoolBinOp DL.OpAnd (SU.negate b) $ SU.negate b'


-- | Desugar boolean arithmetic relations
desugarArithRelation :: ArithRelation -> DL.AExpr -> DL.AExpr ->
                        DL.BExpr
desugarArithRelation OpEq  a a' = DL.ArithRelation DL.OpEq  a a'
desugarArithRelation OpLte a a' = DL.ArithRelation DL.OpLte a a'

-- | Desugar the '!=' (not equal) boolean arithmetic relation
-- (a != a') => !(a = a')
desugarArithRelation OpNeq a a' = SU.negate $ DL.ArithRelation DL.OpEq a a'

-- | Desugar the '>=' (greater or equal) boolean arithmetic relation
-- (a >= a') => (a' <= a)
desugarArithRelation OpGte a a' = DL.ArithRelation DL.OpLte a' a

-- | Desugar the '>' (greater) boolean arithmetic relation
-- (a > a') => !(a <= a')
desugarArithRelation OpGt  a a' = SU.negate $ DL.ArithRelation DL.OpLte a a'

-- | Desugar the '<' (less than) boolean arithmetic relation
-- (a < a') => !(a' <= a)
desugarArithRelation OpLt  a a' = SU.negate $ DL.ArithRelation DL.OpLte a' a
