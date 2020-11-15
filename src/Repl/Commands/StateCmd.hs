module Repl.Commands.StateCmd (
    state
  , state'
) where

import Control.Monad.IO.Class (liftIO)

import Repl.Commands.Utils (
    noArgsAccepted
  , terminate)
import Repl.Types (
  Repl (..)
  , getState
  )


-- | Show the state of the computation
state :: Repl ()
state = do
  s <- getState
  liftIO $ print s
  terminate


state' :: [String] -> Repl ()
state' []    = state
state' (_:_) = liftIO $ putStrLn noArgsAccepted
