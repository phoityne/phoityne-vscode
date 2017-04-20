{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf          #-}

module Phoityne.VSCode.IO.Main (run) where

import qualified Phoityne.VSCode.Argument as A
import qualified Phoityne.VSCode.IO.Control as CTRL

import Data.Either.Utils
import System.Exit
import qualified Control.Exception as E
import qualified Data.ConfigFile as C
import qualified System.Console.CmdArgs as CMD
import qualified System.Log.Logger as L


-- |
-- 
run :: IO Int
run = flip E.catches handlers $ do

  args <- getArgs

  iniSet <- loadIniFile args

  flip E.finally finalProc $ do
    CTRL.run args iniSet

  where
    handlers = [ E.Handler helpExcept
               , E.Handler exitExcept
               , E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers 
    helpExcept (_ :: A.HelpExitException) = return 0 
    exitExcept ExitSuccess                = return 0
    exitExcept (ExitFailure c)            = return c
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1


-- |
-- 
getArgs :: IO A.ArgData
getArgs = E.catches (CMD.cmdArgs A.config) handlers
  where
    handlers = [E.Handler someExcept]
    someExcept (e :: E.SomeException) = if
      | "ExitSuccess" == show e -> E.throw A.HelpExitException
      | otherwise -> E.throwIO e


-- |
-- 
loadIniFile :: A.ArgData
            -> IO C.ConfigParser
loadIniFile _ = do
  let cp = forceEither $ C.readstring C.emptyCP defaultIniSetting
  return cp{ C.accessfunc = C.interpolatingAccess 5 }


-- |
-- 
defaultIniSetting :: String
defaultIniSetting = unlines [
    "[DEFAULT]"
  , "work_dir = ./"
  , ""
  , "[LOG]"
  , "file  = %(work_dir)sphoityne.log"
  , "level = WARNING"
  , ""
  , "[PHOITYNE]"
  ]

