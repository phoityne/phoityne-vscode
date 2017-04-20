{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetFunctionBreakpointsRequestArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.FunctionBreakpointJSON

-- |
--   Arguments for "setFunctionBreakpoints" request.
--
data SetFunctionBreakpointsRequestArguments =
  SetFunctionBreakpointsRequestArguments {
    breakpointsSetFunctionBreakpointsRequestArguments    :: [FunctionBreakpoint]  -- The function names of the breakpoints.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetFunctionBreakpointsRequestArguments") } ''SetFunctionBreakpointsRequestArguments)

