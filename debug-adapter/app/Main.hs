{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Phoityne.VSCode.Main as M
import System.Exit

-- |
--
main :: IO ()
main = do
  M.run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

