-- |
--
-- DesugarStatement defines how to desugar While+ statements.
--

module Desugar.Statement (desugarStmt) where

import Lang
import Desugar.AExpr (desugarAExpr)
import Desugar.BExpr (
    desugarBExpr
  , desugarArithRelation)
import qualified Desugar.Utils as SU
import qualified Desugar.DesugaredLang as DL


-- | Desugar statements
desugarStmt :: Statement -> DL.Statement
desugarStmt (Assignment name s)   = DL.Assignment name $ desugarAExpr s
desugarStmt Skip                  = DL.Skip
desugarStmt (Composition ss)      = DL.Composition $ fmap desugarStmt ss
desugarStmt (Conditional b t f)   = DL.Conditional (desugarBExpr b) (desugarStmt t) $ desugarStmt f
desugarStmt (While b s)           = DL.While (desugarBExpr b) (desugarStmt s)
desugarStmt (Repeat' s b)         = DL.Repeat' (desugarStmt s) (desugarBExpr b)
desugarStmt (Repeat s b)          = desugarRepeatUntilLoop (desugarStmt s) $ desugarBExpr b
desugarStmt (For x a a' s)        = desugarForLoop x (desugarAExpr a) (desugarAExpr a') $ desugarStmt s


-- | Desugar repeat-until statement relying on the while statement.
-- repeat S until b
--   => S; while !b do S
desugarRepeatUntilLoop :: DL.Statement -> DL.BExpr -> DL.Statement
desugarRepeatUntilLoop s b = let
  notB = SU.negate b
  in DL.Composition [s, DL.While notB s]


-- | Desugar for-loop statement relying on the while statement.
-- for x := a to a' do S
--   => x := a; while x < a' do (S; x := x + 1)
desugarForLoop :: Name ->
                  DL.AExpr -> DL.AExpr -> DL.Statement ->
                  DL.Statement
desugarForLoop i start end s = let
  b       = desugarArithRelation OpLt (DL.AVar i) end
  incStmt = DL.ArithBinOp DL.OpSum (DL.AVar i) (DL.NumLit 1)
  in DL.Composition [
      DL.Assignment i start
    , DL.While b $ DL.Composition [
        s
      , DL.Assignment i incStmt]]
