{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.VariableJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Variable is a name/value pair.
--   If the value is structured (has children), a handle is provided to retrieve the children with the VariablesRequest.
--
data Variable =
  Variable {
    nameVariable               :: String  -- The variable's name
  , typeVariable               :: String  -- he variable's type.
  , valueVariable              :: String  -- The variable's value. For structured objects this can be a multi line text, e.g. for a function the body of a function.
  , evaluateNameVariable       :: Maybe String  -- Optional evaluatable name of this variable which can be passed to the 'EvaluateRequest' to fetch the variable's value.
  , variablesReferenceVariable :: Int     -- If variablesReference is > 0, the variable is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Variable") } ''Variable)
