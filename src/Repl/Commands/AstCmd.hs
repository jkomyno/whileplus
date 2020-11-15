module Repl.Commands.AstCmd (
    ast'
  , ast
) where

import Control.Monad.IO.Class (liftIO)

import Parser (parseProgram)
import Pretty (prettyPrint)
import Repl.Commands.Utils (
    withCtxSource
  , terminate)
import Repl.Types (Repl (..))


-- | Parse a line and show the corresponding AST
ast :: String -> Repl ()
ast line = case parseProgram line of
  (Left err)      -> liftIO $ print err
  (Right program) -> prettyPrint program >> terminate


ast' :: [String] -> Repl ()
ast' input = let
  program = unwords input
  isEmpty = null program
  in if isEmpty
    then withCtxSource ast
    else ast program
