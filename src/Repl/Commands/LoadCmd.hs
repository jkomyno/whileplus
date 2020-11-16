module Repl.Commands.LoadCmd (
    load
  , load'
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (
    ExceptT(..)
  , runExceptT)
import Data.Bifunctor (first)
import System.IO.Error (
    tryIOError
  , isDoesNotExistError)

import LangException (LangException(..))
import Parser (parseProgram)
import Repl.Commands.Utils (
    oneArgAccepted
  , terminate)
import Repl.Commands.ResetCmd (reset)
import Repl.Types (
  Repl (..)
  , setLoadedCtx
  , isMemoryModeOn
  )


-- | Load a program source from a file, parse it, and store
-- it in the REPL context
load :: String -> Repl ()
load file = do
  isMemorizing <- isMemoryModeOn
  unless isMemorizing reset

  program <- loadSource file
  case program of
    (Left err)     -> liftIO $ print err
    (Right source) -> do
      case parseProgram source of
        (Left err)  -> liftIO $ print err
        (Right ast) -> do
          setLoadedCtx source ast
          liftIO $ putStrLn $ concat ["File ", show file, " parsed and imported successfully."]
          terminate


load' :: [String] -> Repl ()
load' []  = liftIO $ putStrLn "Filename is required"
load' [f] = load f
load' _   = liftIO $ putStrLn oneArgAccepted


loadSource :: String -> Repl (Either LangException String)
loadSource = liftIO . runExceptT . readFile'

-- | Map an IOError to the corresponding LangException
mapIOError :: String -> IO a -> ExceptT LangException IO a
mapIOError filename = ExceptT . fmap (first mapError) . tryIOError
    where mapError err | isDoesNotExistError err = IONotFoundError filename
          mapError err                           = IOError filename err


readFile' :: FilePath -> ExceptT LangException IO String
readFile' filename = mapIOError filename $ readFile filename
