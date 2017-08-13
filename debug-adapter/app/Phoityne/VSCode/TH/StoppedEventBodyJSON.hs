{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.StoppedEventBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Event message for "stopped" event type.
--   The event indicates that the execution of the debuggee has stopped due to some condition.
--   This can be caused by a break point previously set, a stepping action has completed, by executing a debugger statement etc.
--
data StoppedEventBody =
  StoppedEventBody {
    reasonStoppedEventBody            :: String  -- The reason for the event (such as: 'step', 'breakpoint', 'exception', 'pause'). This string is shown in the UI.
  , threadIdStoppedEventBody          :: Int     -- The thread which was stopped.
  , textStoppedEventBody              :: String  -- Additional information. E.g. if reason is 'exception', text contains the exception name. This string is shown in the UI. 

  -- If allThreadsStopped is true, a debug adapter can announce that all threads have stopped.
  -- The client should use this information to enable that all threads can be expanded to access their stacktraces.
  -- If the attribute is missing or false, only the thread with the given threadId can be expanded.
  , allThreadsStoppedStoppedEventBody :: Bool
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "StoppedEventBody") } ''StoppedEventBody)

defaultStoppedEventBody :: StoppedEventBody
defaultStoppedEventBody = StoppedEventBody "step" 0 "" False

exceptionStoppedEventBody :: String -> StoppedEventBody
exceptionStoppedEventBody msg = StoppedEventBody "exception" 0 msg False
