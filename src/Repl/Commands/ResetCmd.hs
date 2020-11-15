module Repl.Commands.ResetCmd (
    reset
  , reset'
) where

import Control.Monad.IO.Class (liftIO)

import State (empty)
import Repl.Commands.Utils (
    noArgsAccepted
  , terminate)
import Repl.Types (
  Repl (..)
  , setState
  , putStrVerbose
  )

-- reset both the values and functions environment contexts
reset :: Repl ()
reset = do
  setState empty
  putStrVerbose "Reset state:" "OK"
  terminate


reset' :: [String] -> Repl ()
reset' [] = reset
reset' _  = liftIO $ putStrLn noArgsAccepted
