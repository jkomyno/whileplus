module Desugar.Utils (
    negate
  , desugarArithBinOp) where

import Prelude hiding (negate)

import Lang
import qualified Desugar.DesugaredLang as DL

-- | Negate the given desugared boolean expression
negate :: DL.BExpr -> DL.BExpr
negate = DL.BoolUnOp DL.OpNeg


-- | Convert the given arithmetic binary operator in the equivalent
-- desugared version
desugarArithBinOp :: ArithBinOp -> DL.ArithBinOp
desugarArithBinOp OpSum = DL.OpSum
desugarArithBinOp OpMul = DL.OpMul
desugarArithBinOp OpSub = DL.OpSub
