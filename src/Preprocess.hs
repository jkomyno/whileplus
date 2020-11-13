-- |
--
-- Preprocess takes care of examining and transforming a source code string into an
-- interpretable AST.
--
-- The preprocessing steps are the following
-- 1. The source code string is parsed to create a While+ AST.
--    In case of failure, a ParseError exception is returned.
-- 2. The syntactic sugar is removed from the AST
-- 3. The variable declarations and assignment in the desugared AST are examined to
--    determine if they are compatible with an error-free runtime execution.
--    In case of failure, an UnboundVarError exception is returned.
--
-- In case memory mode is turned on, the variable checking step also takes into
-- account a given initial state, which may not necessarily be empty.
--

module Preprocess (preprocess) where

import Check.CheckVariables (checkVariables)
import Check.CheckVariablesWithState (checkVariablesWithState)
import Desugar.DesugaredLang
import LangException (LangException(..))
import Parser (parseProgram)
import Desugar (desugar)
import State (State)

preprocess :: Bool -> State -> String -> Either LangException DesugaredProgram
preprocess isMemoryOn state program = do
  ast <- parseProgram program -- if parsing fails, propagate ParseError
  let desugaredAST = desugar ast
  let checkFn = if isMemoryOn then checkVariablesWithState state else checkVariables
  checkFn desugaredAST -- if variable checking fails, propagate UnboundVarError
  return desugaredAST
