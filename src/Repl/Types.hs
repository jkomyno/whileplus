{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- Repl.Types defines the core types that model the REPL environment, as well as utility
-- functions that operate on those types.
-- The REPL keeps in memory the computation state, an optional context, and some boolean
-- flags that determine whether memory and verbose modes are turned on or off.
--
-- The optional context, if loaded, keeps track of a source code and the corresponding AST
-- parsed from a While+ program.
--
-- The REPL is based on the Repline library, which is turn is based on the popular Haskeline
-- library. The REPL is necessarily based on the IO monad, since it reads from the standard
-- input, is wrapped by a StateT transformer that acts as an internal REPL memory.
--

module Repl.Types (
    ReplState (..)
  , ReplBase
  , Repl (..)
  , StateT
  , MonadState
  , evalStateT
  , initReplState
  , getState
  , setState
  , getLoadedCtxAST
  , getLoadedCtxSource
  , setLoadedCtx
  , toggleVerboseMode
  , toggleMemoryMode
  , isMemoryModeOn
  , printVerbose
  , putStrVerbose
) where

import System.Console.Repline
import Control.Monad (when)
import Control.Monad.State.Strict (
    StateT
  , MonadState
  , get
  , modify'
  , evalStateT
  )
import Control.Monad.IO.Class (liftIO)

import Lang (Program)
import State (
    State
  , empty
  )

-- | Context that may be loaded in the REPL
data LoadedCtx = LoadedCtx
  {
  -- | Source content loaded from a file
    loadedSource :: String

  -- | Parsed AST of loadedSource
  , loadedAST :: Program  }


-- | State carried over by the REPL
data ReplState = ReplState
  {
  -- | Interpreter environment, keeps track of the values associated with the identifiers
    state :: State

  -- | Whether debug mode is active or not
  , isVerbose :: Bool

  -- | Whether memory mode is active or not
  , isMemorizing :: Bool

  -- | Keeps track of the latest loaded context
  , loadedCtx :: Maybe LoadedCtx }


-- | Type of the REPL
type ReplBase = HaskelineT (StateT ReplState IO)
type Repl a = ReplBase a

----------
-- * Utils
----------

-- initially, the REPL state contains and empty environment state, and no file is loaded.
initReplState :: ReplState
initReplState = ReplState {
    state        = empty
  , isVerbose    = False
  , isMemorizing = False
  , loadedCtx    = Nothing }


-- | Extract the interpreter environment from the REPL state
getState :: MonadState ReplState m => m State
getState = do
  ReplState { state } <- get
  return state


-- | Update the REPL state with a new environment state
setState :: MonadState ReplState m => State -> m ()
setState state = do
  modify' (\ReplState { isVerbose, isMemorizing, loadedCtx } ->
    ReplState {
      state = state
    , isVerbose
    , isMemorizing
    , loadedCtx })


-- | Extract the AST from the loaded context
getLoadedCtxAST :: MonadState ReplState m => m (Maybe Program)
getLoadedCtxAST = do
  ReplState { loadedCtx } <- get
  case loadedCtx of
    Nothing    -> return Nothing
    (Just ctx) -> return $ Just $ loadedAST ctx


-- | Extract the source from the loaded context
getLoadedCtxSource :: MonadState ReplState m => m (Maybe String)
getLoadedCtxSource = do
  ReplState { loadedCtx } <- get
  case loadedCtx of
    Nothing    -> return Nothing
    (Just ctx) -> return $ Just $ loadedSource ctx


-- | Update the REPL state with a new loaded context
setLoadedCtx :: MonadState ReplState m => String -> Program -> m ()
setLoadedCtx source ast = do
  let ctx = Just $ LoadedCtx { loadedSource = source, loadedAST = ast }
  modify' (\ReplState { state, isVerbose, isMemorizing } -> ReplState {
      state
    , isVerbose
    , isMemorizing
    , loadedCtx = ctx })


-- | Enable or disable debug mode in REPL state, returning the new state
toggleVerboseMode :: MonadState ReplState m => m Bool
toggleVerboseMode = do
  modify' (\ReplState { state, isVerbose, isMemorizing, loadedCtx } ->
    ReplState {
      state
    , isVerbose = not isVerbose
    , isMemorizing
    , loadedCtx })
  ReplState { isVerbose } <- get
  return isVerbose


-- | Enable or disable memory mode in REPL state, returning new state
toggleMemoryMode :: MonadState ReplState m => m Bool
toggleMemoryMode = do
  modify' (\ReplState { state, isVerbose, isMemorizing, loadedCtx } ->
    ReplState {
      state
    , isVerbose
    , isMemorizing = not isMemorizing
    , loadedCtx })
  ReplState { isMemorizing } <- get
  return isMemorizing


-- | Return whether memory mode is turned on
isMemoryModeOn :: MonadState ReplState m => m Bool
isMemoryModeOn = do
  ReplState { isMemorizing } <- get
  return isMemorizing


-- | Print Show-able value to IO only if the REPL is in debug mode
printVerbose :: (Show a) => String -> a -> Repl ()
printVerbose msg v = do
  ReplState { isVerbose } <- get
  when isVerbose $ do
    liftIO $ putStrLn ""
    liftIO $ putStrLn msg
    liftIO $ print v


-- | Print string value to IO only if the REPL is in debug mode
putStrVerbose :: String -> String -> Repl ()
putStrVerbose msg v = do
  ReplState { isVerbose } <- get
  when isVerbose $ do
    liftIO $ putStrLn ""
    liftIO $ putStr msg
    liftIO $ putStr ""
    liftIO $ putStrLn v
