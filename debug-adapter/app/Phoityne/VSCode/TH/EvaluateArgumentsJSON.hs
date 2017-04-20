{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.EvaluateArgumentsJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Arguments for "evaluate" request.
--
data EvaluateArguments =
  EvaluateArguments {
    expressionEvaluateArguments :: String     -- The expression to evaluate. 
  , frameIdEvaluateArguments    :: Maybe Int  -- Evaluate the expression in the scope of this stack frame. If not specified, the expression is evaluated in the global scope.  
  , contextEvaluateArguments    :: String     -- The context in which the evaluate request is run. Possible values are 'watch' if evaluate is run in a watch, 'repl' if run from the REPL console, or 'hover' if run from a data hover.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "EvaluateArguments") } ''EvaluateArguments)
