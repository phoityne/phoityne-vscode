{-# LANGUAGE TemplateHaskell #-}


module Phoityne.VSCode.TH.InitializeResponseCapabilitesJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility
import Phoityne.VSCode.TH.ExceptionBreakpointsFilterJSON

-- |
--   Information about the capabilities of a debug adapter.
--
data InitializeResponseCapabilites =
  InitializeResponseCapabilites {
    supportsConfigurationDoneRequestInitializeResponseCapabilites  :: Bool  -- The debug adapter supports the configurationDoneRequest.
  , supportsFunctionBreakpointsInitializeResponseCapabilites       :: Bool  -- The debug adapter supports functionBreakpoints.
  , supportsConditionalBreakpointsInitializeResponseCapabilites    :: Bool  -- The debug adapter supports conditionalBreakpoints.
  , supportsHitConditionalBreakpointsInitializeResponseCapabilites :: Bool  -- The debug adapter supports breakpoints that break execution after a specified number of hits.
  , supportsEvaluateForHoversInitializeResponseCapabilites         :: Bool  -- The debug adapter supports a (side effect free) evaluate request for data hovers.
  , exceptionBreakpointFiltersInitializeResponseCapabilites        :: [ExceptionBreakpointsFilter]  -- Available filters for the setExceptionBreakpoints request.
  , supportsStepBackInitializeResponseCapabilites                  :: Bool  -- The debug adapter supports stepping back.
  , supportsSetVariableInitializeResponseCapabilites               :: Bool  -- The debug adapter supports setting a variable to a value.
  , supportsRestartFrameInitializeResponseCapabilites              :: Bool  -- The debug adapter supports restarting a frame.
  , supportsGotoTargetsRequestInitializeResponseCapabilites        :: Bool  -- The debug adapter supports the gotoTargetsRequest.
  , supportsStepInTargetsRequestInitializeResponseCapabilites      :: Bool  -- The debug adapter supports the stepInTargetsRequest. 
  , supportsCompletionsRequestInitializeResponseCapabilites        :: Bool  -- The debug adapter supports the completionsRequest.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "InitializeResponseCapabilites") } ''InitializeResponseCapabilites)

-- |
--
defaultInitializeResponseCapabilites :: InitializeResponseCapabilites
defaultInitializeResponseCapabilites = InitializeResponseCapabilites False False False False False [] False False False False False False

