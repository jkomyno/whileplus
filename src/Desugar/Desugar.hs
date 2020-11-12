{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- |
--
-- Desugar exposes the desugar function, which takes a While+ AST and
-- returns the equivalent desugared AST.
--

module Desugar (desugar) where

import Lang
import qualified Desugar.DesugaredLang as DL
import Desugar.Statement (desugarStmt)

desugar :: Program -> DL.DesugaredProgram
desugar (Program ss) = DL.DesugaredProgram $ fmap desugarStmt ss
