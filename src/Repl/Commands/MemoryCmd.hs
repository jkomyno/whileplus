module Repl.Commands.MemoryCmd (
    memory
  , memory'
) where

import Control.Monad.IO.Class (liftIO)

import Repl.Commands.Utils (
    noArgsAccepted
  , terminate)
import Repl.Types (
  Repl (..)
  , toggleMemoryMode
  )


message :: Bool -> String
message True = "Memory Mode: ON"
message False = "Memory Mode: OFF"


-- | Toggles memory mode in REPL
memory :: Repl ()
memory = do
  isMemoryOn <- toggleMemoryMode
  liftIO $ putStrLn $ message isMemoryOn
  terminate


memory' :: [String] -> Repl ()
memory' [] = memory
memory' _  = liftIO $ putStrLn noArgsAccepted
