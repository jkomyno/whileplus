{-# LANGUAGE BangPatterns #-}

module Repl.Commands.InterpretCmd (
    interpret
  , interpret'
) where

import Control.Monad.IO.Class (liftIO)
import System.Console.Repline (dontCrash)

import Desugar.DesugaredLang (DesugaredProgram)
import Eval.Evaluator (eval)
import Preprocess (preprocess)
import Pretty (prettify)
import Repl.Commands.Utils (
    withCtxSource
  , terminate)
import Repl.Types (
  Repl (..)
  , getState
  , setState
  , isMemoryModeOn
  , printVerbose
  , putStrVerbose)
import State (State)


-- | Parse a line and show the corresponding AST
interpret :: Bool -> State -> String -> Repl ()
interpret isMemoryOn state line = case preprocess isMemoryOn state line of
  (Left err)  -> liftIO $ print err
  (Right ast) -> do
    putStrVerbose "Memory mode: " (if isMemoryOn then "ON" else "OFF")
    putStrVerbose "Desugared AST:\n" $ prettify ast

    -- wrap the evaluation so that in case of user interrupt 
    -- the program doesn't exit abruptly
    dontCrash $ eval' ast state
    terminate


interpret' :: [String] -> Repl ()
interpret' input = let
  program = unwords input
  isEmpty = null program
  in do
    state      <- getState
    isMemoryOn <- isMemoryModeOn
    if isEmpty
      then withCtxSource $ interpret isMemoryOn state
      else interpret isMemoryOn state program


eval' :: DesugaredProgram -> State -> Repl ()
eval' ast state = do
  -- force evaluation, otherwise @eval@ won't be run
  -- when the REPL isn't in verbose mode
  let !state' = eval ast state
  printVerbose "Final state" state'
  setState state'
