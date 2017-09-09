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
import Data.Version


-- |
--
data HelpExitException = HelpExitException
                       deriving (Show, Typeable)

instance E.Exception HelpExitException

-- |
--
data ArgData = ModeA {
                hackageVersion :: String
              } deriving (Data, Typeable, Show, Read, Eq)


-- |
--
config :: ArgData
config = modes [confA]
         &= summary sumMsg
         &= program "phoityne-vscode"
         
  where
    confA = ModeA {
            hackageVersion = showVersion version
            &= name "hackage-version"
            &= typ "VERSION"
            &= explicit
            &= help "hackage module version."
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


