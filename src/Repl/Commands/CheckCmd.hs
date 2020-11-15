module Repl.Commands.CheckCmd (
    check'
  , check
) where

import Control.Monad.IO.Class (liftIO)

import Preprocess (preprocess)
import Repl.Commands.Utils (
    withCtxSource
  , terminate)
import Repl.Types (
    Repl (..)
  , getState
  , isMemoryModeOn
  , printVerbose)
import State (State)


-- | Parse a line and show the corresponding AST
check :: Bool -> State -> String -> Repl ()
check isMemoryOn state line = case preprocess isMemoryOn state line of
  (Left err) -> liftIO $ print err
  (Right _)  -> do
    liftIO $ putStrLn "Check OK."
    terminate


check' :: [String] -> Repl ()
check' input = let
  program = unwords input
  isEmpty = null program
  in do
    state      <- getState
    isMemoryOn <- isMemoryModeOn
    printVerbose "Memory mode: " (if isMemoryOn then "ON" else "OFF")
    if isEmpty
      then withCtxSource $ check isMemoryOn state
      else check isMemoryOn state program
