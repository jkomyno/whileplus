-- |
--
-- Repl exposes the function that runs the While+ REPL environment wrapped in the
-- IO monad.
--

module Repl where

import Data.List (isPrefixOf)
import Control.Monad (unless)
import Control.Monad.IO.Class (
  MonadIO,
  liftIO, )
import System.Console.Repline

import Repl.Commands (
    replCommands
  , replCommandNames
  )
import Repl.Messages (
    headerMessage
  , inputMessage
  )
import Repl.Types (
    Repl (..)
  , initReplState
  , evalStateT
  )


-- | This function is responsible of executing the REPL wrapped in the IO () monad.
-- Internally, the REPL uses the StateT monad transformer, hence it requires the 
-- usage of the 'evalStateT' function. 'initReplState' is the initial REPL state.
-- The REPL only allows the execution of commands that start with the ':' symbol, like GHCI.
repl :: IO ()
repl = flip evalStateT initReplState $
  evalRepl (pure inputMessage) evalCmd replCommands (Just ':')
  (Prefix (wordCompleter byWord) defaultMatcher) header


-- | Handle normal user input. We only allow commands that start with the ':' symbol,
-- and since they're already taken care of by the 'replCommands' function, this function
-- just shows an informative error message
evalCmd :: String -> Repl ()
evalCmd input = unless (null input) *> liftIO $
  putStrLn "Unrecognized input. Try pressing \":\" and TAB.\n"


-- | Show the initial REPL message
header :: Repl ()
header = liftIO $ putStrLn headerMessage


-- | Prefix 'TAB' key completeter.
-- If the user types ':load' and presses the 'TAB' key, the autocompleter will show
-- files in the current directory that can be selected and loaded into the REPL environment.
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
  ]


-- | Default 'TAB' key completer. If the user presses ':' and the 'TAB' key, the autocompleter
-- will show the list of REPL commands
byWord :: Monad m => WordCompleter m
byWord n = do
  return $ filter (isPrefixOf n) replCommandNames
