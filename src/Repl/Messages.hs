{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Repl.Messages defines string contants used in the While+ REPl.
-- 

module Repl.Messages (
  headerMessage,
  inputMessage,
  quitMessage,
  helpMessage,
) where

import Text.RawString.QQ

-- | Message shown as soon as the While+ REPL is started
headerMessage :: String
headerMessage = [r|
                            _   
 _    _  _      _  _      _| |_ 
| |  | || |    (_)| |    |_   _|
| |  | || |__   _ | |  ___ |_|
| |/\| || '_ \ | || | / _ \
\  /\  /| | | || || ||  __/
 \/  \/ |_| |_||_||_| \___|

Welcome to While+ REPL 1.0.0
Copyright (c) 2020 Alberto Schiabel
Released under the BSD3 License

Type :help for help
|]


-- | Message shown at the beginning of the While+ REPL input bar
inputMessage :: String
inputMessage = "While+> "


-- | Message shown when the REPL is closed
quitMessage :: String
quitMessage = "Quitting While+ REPL"


-- | Message shown when the user invokes the 'help' command
helpMessage :: String
helpMessage = [r|whileplus help:
While+ REPL 1.0.0
Copyright (c) 2020 Alberto Schiabel
Released under the BSD3 License


:load FILE            Load a While+ source code from FILE to memory.

:interpret [LINE]     Interpret the given LINE of While+ code.
                      If no LINE is given, it interprets the latest
                      loaded FILE

:ast [LINE]           Show the AST of the given LINE of While+ code.
                      If no LINE is given, it shows the AST of the
                      latest loaded FILE.

:check [LINE]         Check the variable declarations and references
                      in the given LINE of While+ code.
                      If no LINE is given, it checks the variables
                      of the latest loaded FILE.

:desugar [LINE]       Show the desugared AST of the given LINE of
                      While+ code. If no LINE is given, it shows the
                      desugared AST of the latest loaded FILE.

:reset                Reset the interpreter state.

:state                Show the content of the state.

:memory               Toggle memory mode on or off. If memory mode
                      is on, the AST, the desugared AST and the final
                      state of computation is automatically shown.
                      By default, memory mode is off.

:verbose              Toggle verbose mode on or off. If verbose mode
                      is on, the AST, the desugared AST and the final
                      state of computation is automatically shown.
                      By default, verbose mode is off.

:quit                 Quit the While+ language REPL.

:help                 Show this help message.
|]
