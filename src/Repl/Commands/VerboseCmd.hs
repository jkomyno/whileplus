module Repl.Commands.VerboseCmd (
    verbose
  , verbose'
) where

import Control.Monad.IO.Class (liftIO)

import Repl.Commands.Utils (
    noArgsAccepted
  , terminate)
import Repl.Types (
  Repl (..)
  , toggleVerboseMode
  )


message :: Bool -> String
message True = "Verbose Mode: ON"
message False = "Verbose Mode: OFF"


-- | Toggles verbose mode in REPL
verbose :: Repl ()
verbose = do
  isVerboseON <- toggleVerboseMode
  liftIO $ putStrLn $ message isVerboseON
  terminate


verbose' :: [String] -> Repl ()
verbose' [] = verbose
verbose' _  = liftIO $ putStrLn noArgsAccepted
