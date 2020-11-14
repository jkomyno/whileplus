-- |
--
-- Pretty defines utilities to prettify any Show-able instance.
-- This is useful for human-readibility.
--

module Pretty (
    prettify
  , prettyPrint) where

import Control.Monad.IO.Class (
    MonadIO
  , liftIO)
import Text.Nicify (nicify)

prettify :: (Show a) => a -> String
prettify = nicify . show

prettyPrint :: (MonadIO m, Show a) => a -> m ()
prettyPrint = liftIO . putStrLn . prettify
