{-# LANGUAGE TemplateHaskell     #-}


module Phoityne.VSCode.TH.SourceJSON where

import Data.Aeson.TH

import Phoityne.VSCode.Utility

-- |
--   A Source is a descriptor for source code. It is returned from the debug adapter as part of a StackFrame and it is used by clients when specifying breakpoints.
--
data Source =
  Source {
    nameSource            :: Maybe String  -- The short name of the source. Every source returned from the debug adapter has a name. When specifying a source to the debug adapter this name is optional.
  , pathSource            :: String  -- The long (absolute) path of the source. It is not guaranteed that the source exists at this location.
  , sourceReferenceSource :: Maybe Int     -- If sourceReference > 0 the contents of the source can be retrieved through the SourceRequest. A sourceReference is only valid for a session, so it must not be used to persist a source.
  , origineSource         :: Maybe String  -- The (optional) origin of this source: possible values "internal module", "inlined content from source map", etc.
  -- , adapterDataSource  :: Any     -- Optional data that a debug adapter might want to loop through the client. The client should leave the data intact and persist it across sessions. The client should not interpret the data.
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = rdrop (length "Source") } ''Source)
