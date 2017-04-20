{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# OPTIONS_GHC -fno-cse         #-}

module Phoityne.VSCode.Argument (
  HelpExitException(..)
, ArgData(..)
, config
) where

import Paths_phoityne_vscode (version)
import System.Console.CmdArgs
import qualified Control.Exception as E
import Data.Version (showVersion)

-- |
--
data HelpExitException = HelpExitException
                       deriving (Show, Typeable)

instance E.Exception HelpExitException

-- |
--
data ArgData = ModeA deriving (Data, Typeable, Show, Read, Eq)

-- |
--
config :: ArgData
config = modes [confA]
         &= summary sumMsg
         &= program "phoityne-vscode"
         
  where
    confA = ModeA {

          } &= name "ModeA"
            &= details mdAMsg
            &= auto

    sumMsg = unlines [
             "phoityne-vscode-" ++ showVersion version
           ]
           
    mdAMsg = [
             ""
           , "Phoityne is a ghci debug viewer for Visual Studio Code. "
           , ""
           ]


