{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SetBreakpointsRequestArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.SourceJSON
import Phoityne.VSCode.TH.SourceBreakpointJSON

-- |
--   Arguments for "setBreakpoints" request.
--
data SetBreakpointsRequestArguments =
  SetBreakpointsRequestArguments {
    sourceSetBreakpointsRequestArguments         :: Source              --  The source location of the breakpoints; either source.path or source.reference must be specified.
  , breakpointsSetBreakpointsRequestArguments    :: [SourceBreakpoint]  -- The code locations of the breakpoints.
  --, sourceModifiedSetBreakpointsRequestArguments :: Bool                -- A value of true indicates that the underlying source has been modified which results in new breakpoint locations.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "SetBreakpointsRequestArguments") } ''SetBreakpointsRequestArguments)

