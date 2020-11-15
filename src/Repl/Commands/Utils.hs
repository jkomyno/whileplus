module Repl.Commands.Utils (
    noArgsAccepted
  , oneArgAccepted
  , withCtxSource
  , terminate
) where

import Control.Monad.IO.Class (liftIO)

import Repl.Types (
  Repl (..)
  , getLoadedCtxSource
  )


noArgsAccepted :: String
noArgsAccepted = "No arguments are accepted"


oneArgAccepted :: String
oneArgAccepted = "Only one argument is accepted"


withCtxSource :: (String -> Repl ()) -> Repl ()
withCtxSource f = do
  source <- getLoadedCtxSource
  case source of
    Nothing  -> liftIO $ putStrLn "No file loaded.\n"
    (Just x) -> do
      -- liftIO $ putStrLn "Source found."
      f x


terminate :: Repl ()
terminate = liftIO $ putStrLn ""
