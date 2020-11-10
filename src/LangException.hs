{-# OPTIONS_GHC -Wincomplete-patterns #-}

module LangException (
  LangException(..)
) where

import qualified GHC.IO.Exception as E
import Control.Exception (Exception)

-- | LangException models exceptions that may arise parsing or interpreting the language
data LangException
  = UnboundVarError String
  | IOError String E.IOError
  | IONotFoundError String
  | ParseError String
  deriving (Eq)

instance Exception LangException

instance Show LangException where
  -- show :: LangException -> String
  show (UnboundVarError name)     = concat ["UnboundVarError!\n", name, " is not a bounded variable"]
  show (IOError filename msg)     = concat ["IOError!\n", "Error reading file ", filename, ":\n", show msg]
  show (IONotFoundError filename) = concat ["IONotFoundError!\n", "File ", filename, " not found!"]
  show (ParseError msg)           = concat ["ParseError!\n", "Expression can't be parsed: ", msg]
