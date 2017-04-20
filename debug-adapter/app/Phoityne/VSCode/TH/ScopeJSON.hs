{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.ScopeJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Scope is a named container for variables.
--
data Scope =
  Scope {
    nameScope               :: String  -- name of the scope (as such 'Arguments', 'Locals')
  , variablesReferenceScope :: Int     -- The variables of this scope can be retrieved by passing the value of variablesReference to the VariablesRequest. 
  , expensiveScope          :: Bool    -- If true, the number of variables in this scope is large or expensive to retrieve. 
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Scope") } ''Scope)
