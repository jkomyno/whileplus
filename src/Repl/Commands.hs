-- |
--
-- Repl.Commands exposes a list of REPL commands and their human-friendly names.
--

module Repl.Commands (
    replCommands
  , replCommandNames
) where

import Repl.Commands.AstCmd
import Repl.Commands.CheckCmd
import Repl.Commands.DesugarCmd
import Repl.Commands.HelpCmd
import Repl.Commands.InterpretCmd
import Repl.Commands.LoadCmd
import Repl.Commands.MemoryCmd
import Repl.Commands.QuitCmd
import Repl.Commands.ResetCmd
import Repl.Commands.StateCmd
import Repl.Commands.Utils
import Repl.Commands.VerboseCmd
import Repl.Types (Repl (..))

-- | List of REPL commands.
replCommands :: [(String, [String] -> Repl ())]
replCommands = [
    ("load", load')
  , ("interpret", interpret')
  , ("ast", ast')
  , ("check", check')
  , ("desugar", desugar')
  , ("reset", reset')
  , ("state", state')
  , ("memory", memory')
  , ("verbose", verbose')
  , ("quit", quit')
  , ("help", help')
  ]

-- | extract the human-friendly command names from replCommands
replCommandNames :: [String]
replCommandNames = map ((':' :) . fst) replCommands
