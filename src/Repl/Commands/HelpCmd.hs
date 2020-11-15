module Repl.Commands.HelpCmd (
    help
  , help'
) where

import Control.Monad.IO.Class (liftIO)

import Repl.Types (Repl (..))
import Repl.Commands.Utils (terminate)
import Repl.Messages (helpMessage)


help :: Repl ()
help = do
  liftIO $ putStrLn helpMessage
  terminate


help' :: [String] -> Repl ()
help' _ = help
