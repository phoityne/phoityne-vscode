{-# LANGUAGE TemplateHaskell #-}

module Phoityne.VSCode.TH.RequestJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   Client-initiated request
--
data Request =
  Request {
    commandRequest   :: String    -- The command to execute
  -- , argumentsRequest :: [String]  -- Object containing arguments for the command
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Request") } ''Request)
