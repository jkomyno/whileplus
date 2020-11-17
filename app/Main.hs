-- |
--
-- Main is the entrypoint of the whileplus application.
-- On startup, the application loads a REPL (Read-Eval-Print-Loop) environment
-- that exposes a number of commands to read, analyse and interpret While+ programs.
--

module Main where

import Repl (repl)

main :: IO ()
main = repl
