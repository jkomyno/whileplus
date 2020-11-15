module Repl.Commands.DesugarCmd (
    desugar
  , desugar'
) where

import Control.Monad.IO.Class (liftIO)

import Parser (parseProgram)
import Pretty (prettyPrint)
import Repl.Commands.Utils (
    withCtxSource
  , terminate)
import Repl.Types (Repl (..))
import qualified Desugar as D


-- | Parse a line and show the corresponding desugared AST
desugar :: String -> Repl ()
desugar line = case parseProgram line of
  (Left err)  -> liftIO $ print err
  (Right ast) -> prettyPrint (D.desugar ast) >> terminate


desugar' :: [String] -> Repl ()
desugar' input = let
  program = unwords input
  isEmpty = null program
  in if isEmpty
    then withCtxSource desugar
    else desugar program

