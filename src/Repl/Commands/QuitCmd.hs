module Repl.Commands.QuitCmd (
    quit
  , quit'
) where

import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)

import Repl.Commands.Utils (
    noArgsAccepted
  , terminate)
import Repl.Messages (quitMessage)
import Repl.Types (Repl (..))


quit :: Repl ()
quit = do
  liftIO $ putStrLn quitMessage
  terminate >> liftIO exitSuccess


quit' :: [String] -> Repl ()
quit' [] = quit
quit' _  = liftIO $ putStrLn noArgsAccepted
