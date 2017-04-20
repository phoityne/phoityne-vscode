{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.EvaluateBodyJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--    Response to "evaluate" request. 
--
data EvaluateBody =
  EvaluateBody {
    resultEvaluateBody             :: String -- The result of the evaluate.
  , typeEvaluateBody               :: String -- The optional type of the evaluate result. 
  , variablesReferenceEvaluateBody :: Int    -- If variablesReference is > 0, the evaluate result is structured and its children can be retrieved by passing variablesReference to the VariablesRequest
  } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "EvaluateBody") } ''EvaluateBody)

-- |
--
defaultEvaluateBody :: EvaluateBody
defaultEvaluateBody = EvaluateBody "" "" 0



