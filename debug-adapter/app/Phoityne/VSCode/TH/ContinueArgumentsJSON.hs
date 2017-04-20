{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ContinueArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "disconnect" request.
--
data ContinueArguments =
  ContinueArguments {
    threadIdContinueArguments :: Int --  continue execution for this thread.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "ContinueArguments") } ''ContinueArguments)
