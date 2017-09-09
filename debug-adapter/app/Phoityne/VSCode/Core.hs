{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Phoityne.VSCode.Core (
  handleRequest
, DebugContextData(..)
, defaultDebugContextData
, initializeRequestHandler
) where


import qualified Phoityne.GHCi as G

import Paths_phoityne_vscode (version)
import Control.Concurrent
import Control.Monad
import Data.List.Split
import Data.Char
import Safe
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Log.Logger
import Text.Parsec
import qualified Control.Exception as E
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.String.Utils as U
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.FSNotify as FSN
import qualified System.Log.Logger as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import qualified Data.Version as V

import Phoityne.VSCode.Constant
import Phoityne.VSCode.Utility
import qualified Phoityne.VSCode.TH.BreakpointJSON as J
import qualified Phoityne.VSCode.TH.CompletionsItemJSON as J
import qualified Phoityne.VSCode.TH.CompletionsArgumentsJSON as J
import qualified Phoityne.VSCode.TH.CompletionsResponseBodyJSON as J
import qualified Phoityne.VSCode.TH.CompletionsRequestJSON as J
import qualified Phoityne.VSCode.TH.CompletionsResponseJSON as J
import qualified Phoityne.VSCode.TH.ConfigurationDoneRequestJSON as J
import qualified Phoityne.VSCode.TH.ConfigurationDoneResponseJSON as J
import qualified Phoityne.VSCode.TH.ContinueRequestJSON as J
import qualified Phoityne.VSCode.TH.ContinueResponseJSON as J
import qualified Phoityne.VSCode.TH.DisconnectRequestJSON as J
import qualified Phoityne.VSCode.TH.DisconnectResponseJSON as J
import qualified Phoityne.VSCode.TH.EvaluateArgumentsJSON as J
import qualified Phoityne.VSCode.TH.EvaluateBodyJSON as J
import qualified Phoityne.VSCode.TH.EvaluateRequestJSON as J
import qualified Phoityne.VSCode.TH.EvaluateResponseJSON as J
import qualified Phoityne.VSCode.TH.ExceptionBreakpointsFilterJSON as J
import qualified Phoityne.VSCode.TH.InitializedEventJSON as J
import qualified Phoityne.VSCode.TH.InitializeRequestJSON as J
import qualified Phoityne.VSCode.TH.InitializeResponseCapabilitesJSON as J
import qualified Phoityne.VSCode.TH.InitializeResponseJSON as J
import qualified Phoityne.VSCode.TH.LaunchRequestArgumentsJSON as J
import qualified Phoityne.VSCode.TH.LaunchRequestJSON as J
import qualified Phoityne.VSCode.TH.LaunchResponseJSON as J
import qualified Phoityne.VSCode.TH.NextRequestJSON as J
import qualified Phoityne.VSCode.TH.NextResponseJSON as J
import qualified Phoityne.VSCode.TH.OutputEventJSON as J
import qualified Phoityne.VSCode.TH.OutputEventBodyJSON as J
import qualified Phoityne.VSCode.TH.PauseRequestJSON as J
import qualified Phoityne.VSCode.TH.PauseResponseJSON as J
import qualified Phoityne.VSCode.TH.RequestJSON as J
import qualified Phoityne.VSCode.TH.ScopesArgumentsJSON as J
import qualified Phoityne.VSCode.TH.ScopesRequestJSON as J
import qualified Phoityne.VSCode.TH.ScopesResponseJSON as J
import qualified Phoityne.VSCode.TH.SetBreakpointsRequestArgumentsJSON as J
import qualified Phoityne.VSCode.TH.SetBreakpointsRequestJSON as J
import qualified Phoityne.VSCode.TH.SetBreakpointsResponseBodyJSON as J
import qualified Phoityne.VSCode.TH.SetBreakpointsResponseJSON as J
import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsRequestArgumentsJSON as J
import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsRequestJSON as J
import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsResponseBodyJSON as J
import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsResponseJSON as J
import qualified Phoityne.VSCode.TH.SetExceptionBreakpointsRequestArgumentsJSON as J
import qualified Phoityne.VSCode.TH.SetExceptionBreakpointsRequestJSON as J
import qualified Phoityne.VSCode.TH.SetExceptionBreakpointsResponseJSON as J
import qualified Phoityne.VSCode.TH.SourceBreakpointJSON as J
import qualified Phoityne.VSCode.TH.FunctionBreakpointJSON as J
import qualified Phoityne.VSCode.TH.SourceJSON as J
import qualified Phoityne.VSCode.TH.SourceRequestJSON as J
import qualified Phoityne.VSCode.TH.SourceResponseJSON as J
import qualified Phoityne.VSCode.TH.StackFrameJSON as J
import qualified Phoityne.VSCode.TH.StackTraceBodyJSON as J
import qualified Phoityne.VSCode.TH.StackTraceRequestJSON as J
import qualified Phoityne.VSCode.TH.StackTraceResponseJSON as J
import qualified Phoityne.VSCode.TH.StepInRequestJSON as J
import qualified Phoityne.VSCode.TH.StepInResponseJSON as J
import qualified Phoityne.VSCode.TH.StepOutRequestJSON as J
import qualified Phoityne.VSCode.TH.StepOutResponseJSON as J
import qualified Phoityne.VSCode.TH.StoppedEventJSON as J
import qualified Phoityne.VSCode.TH.StoppedEventBodyJSON as J
import qualified Phoityne.VSCode.TH.TerminatedEventJSON as J
import qualified Phoityne.VSCode.TH.TerminatedEventBodyJSON as J
import qualified Phoityne.VSCode.TH.ThreadsRequestJSON as J
import qualified Phoityne.VSCode.TH.ThreadsResponseJSON as J
import qualified Phoityne.VSCode.TH.VariableJSON as J
import qualified Phoityne.VSCode.TH.VariablesBodyJSON as J
import qualified Phoityne.VSCode.TH.VariablesRequestJSON as J
import qualified Phoityne.VSCode.TH.VariablesResponseJSON as J

-- |
--
--
data DebugContextData = 
  DebugContextData {
    resSeqDebugContextData                  :: Int
  , functionBreakPointDatasDebugContextData :: BreakPointDatas
  , breakPointDatasDebugContextData         :: BreakPointDatas
  , workspaceDebugContextData               :: FilePath
  , startupDebugContextData                 :: FilePath
  , debugStartedDebugContextData            :: Bool
  , debugStoppedPosDebugContextData         :: Maybe G.SourcePosition
  , currentFrameIdDebugContextData          :: Int
  , modifiedDebugContextData                :: Bool
  , ghciProcessDebugContextData             :: Maybe G.GHCiProcess
  , responseHandlerDebugContextData         :: BSL.ByteString -> IO ()
  , stopOnEntryDebugContextData             :: Bool
  , hackagePackageVersionDebugContextData   :: String 
  }


-- |
--  
-- 
data BreakPointData =
  BreakPointData {
    nameBreakPointData :: String
  , srcPosBreakPointData       :: G.SourcePosition
  , breakNoBreakPointData      :: Maybe Int
  , conditionBreakPointData    :: Maybe String
  , hitConditionBreakPointData :: Maybe String
  , hitCountBreakPointData     :: Int
  } deriving (Show, Read, Eq, Ord)


-- |
--  
-- 
type BreakPointDataKey = G.SourcePosition

-- |
--  
-- 
type BreakPointDatas = M.Map BreakPointDataKey BreakPointData

-- |
--  
-- 
getBreakPointKey :: BreakPointData -> BreakPointDataKey
getBreakPointKey = srcPosBreakPointData


-- |
--
incrementBreakPointHitCount :: BreakPointData -> BreakPointData
incrementBreakPointHitCount bp = bp{hitCountBreakPointData = 1 + hitCountBreakPointData bp}



-- |
--
--
_INITIAL_RESPONSE_SEQUENCE :: Int
_INITIAL_RESPONSE_SEQUENCE = 0


-- |
--
--
_SEP_WIN :: Char
_SEP_WIN = '\\'

-- |
--
--
_SEP_UNIX :: Char
_SEP_UNIX = '/'

-- |
--
--
_TASKS_JSON_FILE_CONTENTS :: BSL.ByteString
_TASKS_JSON_FILE_CONTENTS = str2lbs $ U.join "\n" $
  [
    "{"
  , "  // atuomatically created by phoityne-vscode"
  , "  "
  , "  \"version\": \"0.1.0\","
  , "  \"isShellCommand\": true,"
  , "  \"showOutput\": \"always\","
  , "  \"suppressTaskName\": true,"
  , "  \"windows\": {"
  , "    \"command\": \"cmd\","
  , "    \"args\": [\"/c\"]"
  , "  },"
  , "  \"linux\": {"
  , "    \"command\": \"sh\","
  , "    \"args\": [\"-c\"]"
  , "  },"
  , "  \"osx\": {"
  , "    \"command\": \"sh\","
  , "    \"args\": [\"-c\"]"
  , "  },"
  , "  \"tasks\": ["
  , "    {"
  , "       \"taskName\": \"stack build\","
  , "       \"args\": [ \"echo START_STACK_BUILD && cd ${workspaceRoot} && stack build && echo END_STACK_BUILD \" ]"
  , "    },"
  , "    { "
  , "       \"isBuildCommand\": true,"
  , "       \"taskName\": \"stack clean & build\","
  , "       \"args\": [ \"echo START_STACK_CLEAN_AND_BUILD && cd ${workspaceRoot} && stack clean && stack build && echo END_STACK_CLEAN_AND_BUILD \" ]"
  , "    },"
  , "    { "
  , "       \"isTestCommand\": true,"
  , "       \"taskName\": \"stack test\","
  , "       \"args\": [ \"echo START_STACK_TEST && cd ${workspaceRoot} && stack test && echo END_STACK_TEST \" ]"
  , "    },"
  , "    { "
  , "       \"isBackground\": true,"
  , "       \"taskName\": \"stack watch\","
  , "       \"args\": [ \"echo START_STACK_WATCH && cd ${workspaceRoot} && stack build --test --no-run-tests --file-watch && echo END_STACK_WATCH \" ]"
  , "    }"
  , "  ]"
  , "}"
  ]


-- |
--
--
_ERR_MSG_URL :: [String]
_ERR_MSG_URL = [ "`stack update` and install new phoityen-vscode."
               , "Or check information on https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode"
               ]


-- |
--
--
_DEBUG_START_MSG :: [String]
_DEBUG_START_MSG = [
    ""
  , "  Now, ghci launched and configuration done."
  , "  Press F5 to start debugging."
  , "  Or modify source code. it will be loaded to ghci automatically."
  , " "
  ]


-- |
--
--
_NEW_VERSION_MSG :: [String]
_NEW_VERSION_MSG = [
    ""
  , "  New hackage module has been released."
  , "  `stack update` and install new phoityen-vscode."
  , " "
  ]


-- |
--
--
_NOT_PERMIT_REPL_COMMANDS :: [String]
_NOT_PERMIT_REPL_COMMANDS = [
    ":{"
  , ":abandon", ":back", ":break", ":continue", ":delete", ":force",     ":forward"
  , ":history", ":list", ":print", ":sprint",   ":step",   ":steplocal", ":stepmodule", ":trace"
  ]

-- |
--
--
defaultDebugContextData :: DebugContextData
defaultDebugContextData = DebugContextData {
    resSeqDebugContextData                  = _INITIAL_RESPONSE_SEQUENCE
  , functionBreakPointDatasDebugContextData = M.fromList []
  , breakPointDatasDebugContextData         = M.fromList []
  , workspaceDebugContextData               = ""
  , startupDebugContextData                 = ""
  , debugStartedDebugContextData            = False
  , debugStoppedPosDebugContextData         = Nothing
  , currentFrameIdDebugContextData          = 0
  , modifiedDebugContextData                = False
  , ghciProcessDebugContextData             = Nothing
  , responseHandlerDebugContextData         = BSL.putStr
  , stopOnEntryDebugContextData             = False
  , hackagePackageVersionDebugContextData   = ""
  }


-- |=====================================================================
--  Request / Response / Event
-- 


-- |
--
--
logRequest :: String -> IO ()
logRequest reqStr = infoM _LOG_NAME $ "[REQUEST] " ++ reqStr


-- |
--
isLoggingStarted :: MVar DebugContextData -> IO Bool
isLoggingStarted mvarCtx = do
  ctx <- readMVar mvarCtx
  return . not .  null $ workspaceDebugContextData ctx 

-- |
--
sendResponse :: MVar DebugContextData -> BSL.ByteString -> IO ()
sendResponse mvarCtx str = do
  doLog <- isLoggingStarted mvarCtx
  when doLog $ infoM _LOG_NAME $ "[RESPONSE]" ++ lbs2str str

  ctx <- readMVar mvarCtx
  responseHandlerDebugContextData ctx str


-- |
--
--
sendConsoleEvent :: MVar DebugContextData -> String -> IO ()
sendConsoleEvent mvarCtx msg = sendOutputEventWithType mvarCtx msg "console"


-- |
--
--
sendStdoutEvent :: MVar DebugContextData -> String -> IO ()
sendStdoutEvent mvarCtx msg = sendOutputEventWithType mvarCtx msg "stdout"


-- |
--
--
sendErrorEvent :: MVar DebugContextData -> String -> IO ()
sendErrorEvent mvarCtx msg = do
  doLog <- isLoggingStarted mvarCtx
  when doLog $ errorM _LOG_NAME msg
  sendOutputEventWithType mvarCtx msg "stderr"


-- |
--
--
sendOutputEventWithType :: MVar DebugContextData -> String -> String -> IO ()
sendOutputEventWithType mvarCtx msg msgType= do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let outEvt    = J.defaultOutputEvent resSeq
      outEvtStr = J.encode outEvt{J.bodyOutputEvent = J.OutputEventBody msgType msg Nothing }
  sendEvent mvarCtx outEvtStr


-- |
--
sendEvent :: MVar DebugContextData -> BSL.ByteString -> IO ()
sendEvent mvarCtx str =  do
  doLog <- isLoggingStarted mvarCtx
  when doLog $ infoM _LOG_NAME $ "[EVENT]" ++ lbs2str str

  ctx <- readMVar mvarCtx
  responseHandlerDebugContextData ctx str


-- |
--
sendTerminateEvent :: MVar DebugContextData -> IO ()
sendTerminateEvent mvarDat = do
  resSeq <- getIncreasedResponseSequence mvarDat
  let terminatedEvt    = J.defaultTerminatedEvent resSeq
      terminatedEvtStr = J.encode terminatedEvt
  sendEvent mvarDat terminatedEvtStr

-- |
--
sendRestartEvent :: MVar DebugContextData -> IO ()
sendRestartEvent mvarCtx = do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let terminatedEvt    = J.defaultTerminatedEvent resSeq
      terminatedEvtStr = J.encode terminatedEvt{J.bodyTerminatedEvent = J.TerminatedEventBody True}
  sendEvent mvarCtx terminatedEvtStr


-- |=====================================================================
--  Handlers

-- |
--
--
handleRequest :: MVar DebugContextData -> BSL.ByteString -> BSL.ByteString -> IO ()
handleRequest mvarDat contLenStr jsonStr = do
  case J.eitherDecode jsonStr :: Either String J.Request of
    Right (J.Request cmd) -> handle cmd
    Left  err -> sendParseErrorAndTerminate err "request"

  where
    sendParseErrorAndTerminate err typ = do
      let msg =  L.intercalate "\n"
              $ [  "[CRITICAL]"++"<"++typ++">"++" request parce error."
                ,  lbs2str contLenStr
                ,  lbs2str jsonStr
                ,  show err, ""
                ] ++ _ERR_MSG_URL ++ ["", ""]
      sendErrorEvent mvarDat msg
      sendTerminateEvent mvarDat

    handle "initialize" = case J.eitherDecode jsonStr :: Either String J.InitializeRequest of
      Right req -> initializeRequestHandler mvarDat req
      Left  err -> do
        let cont = L.intercalate " " $ map U.strip $ lines $ lbs2str contLenStr
            json = L.intercalate " " $ map U.strip $ lines $ lbs2str jsonStr
            er   = L.intercalate " " $ map U.strip $ lines $ show err
            msg  = L.intercalate " " $ ["[CRITICAL]<initialize> request parce error.", cont, json, er] ++ _ERR_MSG_URL
        resSeq <- getIncreasedResponseSequence mvarDat
        sendResponse mvarDat $ J.encode $ J.parseErrorInitializeResponse resSeq msg
        sendParseErrorAndTerminate err "initialize"

    handle "launch" = case J.eitherDecode jsonStr :: Either String J.LaunchRequest of
      Right req -> launchRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "launch"

    handle "configurationDone" = case J.eitherDecode jsonStr :: Either String J.ConfigurationDoneRequest of
      Right req -> configurationDoneRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "configurationDone" 

    handle "disconnect" = case J.eitherDecode jsonStr :: Either String J.DisconnectRequest of
      Right req -> disconnectRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "disconnect"

    handle "setBreakpoints" = case J.eitherDecode jsonStr :: Either String J.SetBreakpointsRequest of
      Right req -> setBreakpointsRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "setBreakpoints"

    handle "setFunctionBreakpoints" = case J.eitherDecode jsonStr :: Either String J.SetFunctionBreakpointsRequest of
      Right req -> setFunctionBreakpointsRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "setFunctionBreakpoints"

    handle "setExceptionBreakpoints" = case J.eitherDecode jsonStr :: Either String J.SetExceptionBreakpointsRequest of
      Right req -> setExceptionBreakpointsRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "setExceptionBreakpoints"

    handle "continue" = case J.eitherDecode jsonStr :: Either String J.ContinueRequest of
      Right req -> continueRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "continue"

    handle "next" = case J.eitherDecode jsonStr :: Either String J.NextRequest of
      Right req -> nextRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "next"

    handle "stepIn" = case J.eitherDecode jsonStr :: Either String J.StepInRequest of
      Right req -> stepInRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "stepIn"

    handle "stackTrace" = case J.eitherDecode jsonStr :: Either String J.StackTraceRequest of
      Right req -> stackTraceRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "stackTrace"

    handle "scopes" = case J.eitherDecode jsonStr :: Either String J.ScopesRequest of
      Right req -> scopesRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "scopes"

    handle "variables" = case J.eitherDecode jsonStr :: Either String J.VariablesRequest of
      Right req -> variablesRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "variables"

    handle "threads" = case J.eitherDecode jsonStr :: Either String J.ThreadsRequest of
      Right req -> threadsRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "threads"

    handle "evaluate" = case J.eitherDecode jsonStr :: Either String J.EvaluateRequest of
      Right req -> evaluateRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "evaluate"

    handle "completions" = case J.eitherDecode jsonStr :: Either String J.CompletionsRequest of
      Right req -> completionsRequestHandler mvarDat req
      Left  err -> sendParseErrorAndTerminate err "completions"

    -- |
    --  not supported.
    --
    handle "stepOut" = case J.eitherDecode jsonStr :: Either String J.StepOutRequest of
      Left  err -> sendParseErrorAndTerminate err "stepOut"
      Right req -> do
        resSeq <- getIncreasedResponseSequence mvarDat
        let res    = J.defaultStepOutResponse resSeq req
            resStr = J.encode $ res{J.successStepOutResponse = False, J.messageStepOutResponse = "unsupported command."}
        sendResponse mvarDat resStr
        sendErrorEvent mvarDat "[WARN] stepOut command is not supported. ignored."

    handle "pause" = case J.eitherDecode jsonStr :: Either String J.PauseRequest of
      Left  err -> sendParseErrorAndTerminate err "pause"
      Right req -> do
        resSeq <- getIncreasedResponseSequence mvarDat
        let res    = J.defaultPauseResponse resSeq req
            resStr = J.encode $ res{J.successPauseResponse = False, J.messagePauseResponse = "unsupported command."}
        sendResponse mvarDat resStr
 
        sendErrorEvent  mvarDat "[WARN] pause command is not supported. ignored."

    handle "source" = case J.eitherDecode jsonStr :: Either String J.SourceRequest of
      Left  err -> sendParseErrorAndTerminate err "source"
      Right req -> do
        resSeq <- getIncreasedResponseSequence mvarDat
        let res    = J.defaultSourceResponse resSeq req
            resStr = J.encode $ res{J.successSourceResponse = False, J.messageSourceResponse = "unsupported command."}
        sendResponse mvarDat resStr
 
        sendErrorEvent mvarDat "[WARN] source command is not supported. ignored."

    handle cmd = do
      let msg = L.intercalate " " ["[WARN] unknown request command. ignored.", cmd, lbs2str contLenStr, lbs2str jsonStr]
      sendErrorEvent mvarDat msg


-- |
--
initializeRequestHandler :: MVar DebugContextData -> J.InitializeRequest -> IO ()
initializeRequestHandler mvarCtx req@(J.InitializeRequest seq _ _ _) = flip E.catches handlers $ do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let capa = J.InitializeResponseCapabilites {
             J.supportsConfigurationDoneRequestInitializeResponseCapabilites  = True
           , J.supportsFunctionBreakpointsInitializeResponseCapabilites       = True
           , J.supportsConditionalBreakpointsInitializeResponseCapabilites    = True
           , J.supportsHitConditionalBreakpointsInitializeResponseCapabilites = True
           , J.supportsEvaluateForHoversInitializeResponseCapabilites         = True
           , J.exceptionBreakpointFiltersInitializeResponseCapabilites        = [
                 J.ExceptionBreakpointsFilter "break-on-error" "break-on-error" False
               , J.ExceptionBreakpointsFilter "break-on-exception" "break-on-exception" False
               ]
           , J.supportsStepBackInitializeResponseCapabilites                  = False
           , J.supportsSetVariableInitializeResponseCapabilites               = False
           , J.supportsRestartFrameInitializeResponseCapabilites              = False
           , J.supportsGotoTargetsRequestInitializeResponseCapabilites        = False
           , J.supportsStepInTargetsRequestInitializeResponseCapabilites      = False
           , J.supportsCompletionsRequestInitializeResponseCapabilites        = True
           }
      res  = J.InitializeResponse resSeq "response" seq True "initialize" "" capa

  sendResponse mvarCtx $ J.encode res

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["initialize request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorInitializeResponse resSeq req msg 
      sendErrorEvent mvarCtx msg


-- |
--
launchRequestHandler :: MVar DebugContextData -> J.LaunchRequest -> IO ()
launchRequestHandler mvarCtx req@(J.LaunchRequest _ _ _ args) = flip E.catches handlers $ do

  ctx <- takeMVar mvarCtx
  putMVar mvarCtx ctx {
      workspaceDebugContextData   = J.workspaceLaunchRequestArguments args
    , startupDebugContextData     = J.startupLaunchRequestArguments args
    , stopOnEntryDebugContextData = J.stopOnEntryLaunchRequestArguments args
    }

  let logLevelStr = J.logLevelLaunchRequestArguments args
  logLevel <- case readMay logLevelStr of
    Just lv -> return lv
    Nothing -> do
      sendErrorEvent mvarCtx $ "log priority is invalid. WARNING set. [" ++ logLevelStr ++ "]\n"
      return WARNING

  setupLogger (J.logFileLaunchRequestArguments args) logLevel

  logRequest $ show req

  prepareTasksJsonFile


  runGHCi mvarCtx
          (J.ghciCmdLaunchRequestArguments args)
          (J.ghciPromptLaunchRequestArguments args)
          (J.ghciEnvLaunchRequestArguments args)
           >>= ghciLaunched 

    
  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["launch request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorLaunchResponse resSeq req msg 
      sendErrorEvent mvarCtx $ msg ++ "\n"

    -- |
    -- 
    prepareTasksJsonFile = do
      ctx <- readMVar mvarCtx
      let jsonFile = workspaceDebugContextData ctx </> ".vscode" </> "tasks.json"
    
      doesFileExist jsonFile >>= \case
        True  -> debugM _LOG_NAME $ "tasks.json file exists. " ++ jsonFile 
        False -> do
          sendConsoleEvent mvarCtx $ "create tasks.json file. " ++ jsonFile ++ "\n"
          saveFileLBS jsonFile _TASKS_JSON_FILE_CONTENTS

    -- |
    -- 
    ghciLaunched (Left err) = do
      let msg = L.intercalate " " ["ghci launch error.", err]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorLaunchResponse resSeq req msg 
      sendErrorEvent mvarCtx $ msg ++ "\n"

      sendTerminateEvent mvarCtx


    ghciLaunched (Right ghciProc) = do
      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{ghciProcessDebugContextData = Just ghciProc}

      startupRes <- loadHsFile mvarCtx (J.startupLaunchRequestArguments args)

      when (False == startupRes) $ do
        let msg = L.intercalate " " ["startup load error.", J.startupLaunchRequestArguments args]
        sendErrorEvent mvarCtx $ msg ++ "\n"

      setMainArgs ghciProc (J.mainArgsLaunchRequestArguments args) >>= \case
        Right _  -> return ()
        Left err -> do
          let msg = L.intercalate " " ["set args error.", err, show (J.mainArgsLaunchRequestArguments args)]
          sendErrorEvent mvarCtx $ msg ++ "\n"

      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.defaultLaunchResponse resSeq req
    
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendEvent mvarCtx $ J.encode $ J.defaultInitializedEvent resSeq

      watch mvarCtx
    
    setMainArgs _ Nothing = return . Right $ ()
    setMainArgs ghciProc (Just argsStr)
      | null (U.strip argsStr) = return . Right $ ()
      | otherwise = G.set ghciProc (sendStdoutEvent mvarCtx) ("args " ++ argsStr)


-- |
--
configurationDoneRequestHandler :: MVar DebugContextData -> J.ConfigurationDoneRequest -> IO ()
configurationDoneRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["configurationDone request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorConfigurationDoneResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    withProcess Nothing = sendErrorEvent mvarCtx "[withProcess] ghci not started."

    withProcess (Just ghciProc) = do
      sendConsoleEvent mvarCtx $ L.intercalate "\n" _DEBUG_START_MSG

      checkVersion

      sendStdoutEvent mvarCtx $ G.promptGHCiProcess ghciProc

      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.defaultConfigurationDoneResponse resSeq req

      stopOnEntryDebugContextData <$> (readMVar mvarCtx) >>= stopOnEntry 

    stopOnEntry False = proceedDebug mvarCtx
    stopOnEntry True = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let stopEvt    = J.defaultStoppedEvent resSeq
          stopEvtStr = J.encode stopEvt
      sendEvent mvarCtx stopEvtStr

    checkVersion = do
      verStr <- hackagePackageVersionDebugContextData <$> (readMVar mvarCtx) 
      verArg <- case getVersion verStr of
        Right v  -> return v
        Left err -> do
          sendErrorEvent mvarCtx $ "[checkVersion] argument version parse error. " ++ err
          return version

      when (version < verArg) $ do
        sendErrorEvent mvarCtx $  L.intercalate "\n" _NEW_VERSION_MSG
        
    getVersion :: String -> Either String V.Version
    getVersion str = case parse getVersionParser "getVersionParser" str of
      Right v -> Right v
      Left e  -> Left $ "can not parse hackage module version. " ++ show e

    getVersionParser = do
      v1 <- manyTill digit (char '.')
      v2 <- manyTill digit (char '.')
      v3 <- manyTill digit (char '.')
      v4 <- manyTill digit eof
      return $ V.makeVersion [read v1, read v2, read v3, read v4]

-- |
--
disconnectRequestHandler :: MVar DebugContextData -> J.DisconnectRequest -> IO ()
disconnectRequestHandler mvarCtx req = do
  logRequest $ show req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess
  exitSuccess

  where
    withProcess Nothing = do
      sendErrorEvent mvarCtx "[disconnectRequestHandler] ghci not started."
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.defaultDisconnectResponse resSeq req

    withProcess (Just ghciProc) = G.quit ghciProc outHdl >>= withExitCode

    withExitCode (Left err)  = do
      sendErrorEvent mvarCtx $ "[disconnectRequestHandler] ghci quit error. " ++ err
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.defaultDisconnectResponse resSeq req

    withExitCode (Right code) = do
      sendStdoutEvent mvarCtx $ show code
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.defaultDisconnectResponse resSeq req

    outHdl = sendStdoutEvent mvarCtx

-- |
--
setBreakpointsRequestHandler :: MVar DebugContextData -> J.SetBreakpointsRequest -> IO ()
setBreakpointsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  let cwd     = workspaceDebugContextData ctx
      args    = J.argumentsSetBreakpointsRequest req
      source  = J.sourceSetBreakpointsRequestArguments args
      path    = J.pathSource source
      reqBps  = J.breakpointsSetBreakpointsRequestArguments args
      bps     = map (convBp cwd path) reqBps

  delete path
  resBody <- insert bps

  resSeq <- getIncreasedResponseSequence mvarCtx
  let res    = J.defaultSetBreakpointsResponse resSeq req
      resStr = J.encode res{J.bodySetBreakpointsResponse = J.SetBreakpointsResponseBody resBody}
  sendResponse mvarCtx resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["setBreakpoints request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorSetBreakpointsResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    convBp cwd path (J.SourceBreakpoint lineNo colNo cond hitCond) =
      BreakPointData {
        nameBreakPointData       = src2mod cwd path
      , srcPosBreakPointData     = G.SourcePosition path lineNo (maybe (-1) id colNo) (-1) (-1)
      , breakNoBreakPointData    = Nothing
      , conditionBreakPointData  = normalizeCond cond
      , hitConditionBreakPointData = hitCond
      , hitCountBreakPointData     = 0
      }

    normalizeCond Nothing = Nothing
    normalizeCond (Just c)
      | null (U.strip c) = Nothing
      | otherwise = Just c

    delete path = do
      ctx <- takeMVar mvarCtx
      let bps = breakPointDatasDebugContextData ctx
          newBps = M.filterWithKey (\(G.SourcePosition p _ _ _ _) _-> path /= p) bps
          delBps = M.elems $ M.filterWithKey (\(G.SourcePosition p _ _ _ _) _-> path == p) bps

      putMVar mvarCtx ctx{breakPointDatasDebugContextData = newBps}

      debugM _LOG_NAME $ "del bps:" ++ show delBps

      mapM_ (deleteBreakPointOnGHCi mvarCtx) delBps


    insert reqBps = do
      results <- mapM insertInternal reqBps
      let addBps  = filter (\(_, (J.Breakpoint _ verified _ _ _ _ _ _)) -> verified) results
          resData = map snd results

      debugM _LOG_NAME $ "add bps:" ++ show addBps
      debugM _LOG_NAME $ "response bps:" ++ show resData

      ctx <- takeMVar mvarCtx
      let bps    = breakPointDatasDebugContextData ctx
          newBps = foldr (\v@(BreakPointData _ s _ _ _ _)->M.insert s v) bps $ map fst results
      putMVar mvarCtx ctx{breakPointDatasDebugContextData = newBps}

      return resData


    insertInternal reqBp@(BreakPointData modName (G.SourcePosition filePath lineNo _ _ _) _ _ _ _) =
      addBreakPointOnGHCi mvarCtx reqBp >>= \case
        Right (no, srcPos@(G.SourcePosition path sl sc el ec)) ->
          return (reqBp{ breakNoBreakPointData   = Just no
                       , srcPosBreakPointData    = srcPos
                       },
                  J.Breakpoint (Just no) True ""
                    (J.Source (Just modName) path Nothing Nothing) sl sc el ec)
        Left err ->
          return (reqBp,
                  J.Breakpoint Nothing False err
                    (J.Source (Just modName) filePath Nothing Nothing)
                    lineNo (-1) lineNo (-1))

-- |
--
setFunctionBreakpointsRequestHandler :: MVar DebugContextData -> J.SetFunctionBreakpointsRequest -> IO ()
setFunctionBreakpointsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let args    = J.argumentsSetFunctionBreakpointsRequest req
      reqBps  = J.breakpointsSetFunctionBreakpointsRequestArguments args
      bps     = map convBp reqBps

  delete
  resBody <- insert bps

  resSeq <- getIncreasedResponseSequence mvarCtx
  let res    = J.defaultSetFunctionBreakpointsResponse resSeq req
      resStr = J.encode res{J.bodySetFunctionBreakpointsResponse = J.SetFunctionBreakpointsResponseBody resBody}
  sendResponse mvarCtx resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["setFunctionBreakpoints request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorSetFunctionBreakpointsResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    convBp (J.FunctionBreakpoint name cond hitCond) =
      BreakPointData {
        nameBreakPointData         = name
      , srcPosBreakPointData       = G.SourcePosition "" (-1) (-1) (-1) (-1)
      , breakNoBreakPointData      = Nothing
      , conditionBreakPointData    = normalizeCond cond
      , hitConditionBreakPointData = hitCond
      , hitCountBreakPointData     = 0
      }

    normalizeCond Nothing = Nothing
    normalizeCond (Just c)
      | null (U.strip c) = Nothing
      | otherwise = Just c

    delete = do
      ctx <- takeMVar mvarCtx
      let bps = functionBreakPointDatasDebugContextData ctx
          delBps = M.elems bps

      putMVar mvarCtx ctx{functionBreakPointDatasDebugContextData = M.fromList []}

      debugM _LOG_NAME $ "del bps:" ++ show delBps

      mapM_ (deleteBreakPointOnGHCi mvarCtx) delBps


    insert reqBps = do
      results <- mapM insertInternal reqBps
      let addBps  = filter (\(_, (J.Breakpoint _ verified _ _ _ _ _ _)) -> verified) results
          resData = map snd results

      debugM _LOG_NAME $ "add funBPs:" ++ show addBps
      debugM _LOG_NAME $ "response funBPs:" ++ show resData

      ctx <- takeMVar mvarCtx
      let bps    = functionBreakPointDatasDebugContextData ctx
          newBps = foldr (\v@(BreakPointData _ s _ _ _ _)->M.insert s v) bps $ map fst results
      putMVar mvarCtx ctx{functionBreakPointDatasDebugContextData = newBps}

      return resData


    insertInternal reqBp@(BreakPointData funcName _ _ _ _ _) = do
      addFunctionBreakPointOnGHCi mvarCtx reqBp >>= \case
        Right (no, srcPos@(G.SourcePosition path sl sc el ec)) -> do
          return ( reqBp{ breakNoBreakPointData   = Just no
                        , srcPosBreakPointData    = srcPos
                        }
                 , J.Breakpoint (Just no) True "" (J.Source (Just funcName) path Nothing Nothing) sl sc el ec)
        Left err -> return (reqBp, J.Breakpoint Nothing False err (J.Source (Just funcName) "" Nothing Nothing) (-1) (-1) (-1) (-1))

-- |
--
setExceptionBreakpointsRequestHandler :: MVar DebugContextData -> J.SetExceptionBreakpointsRequest -> IO ()
setExceptionBreakpointsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let args    = J.argumentsSetExceptionBreakpointsRequest req
      filters = J.filtersSetExceptionBreakpointsRequestArguments args
  
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess filters

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["setExceptionBreakpoints request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorSetExceptionBreakpointsResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    withProcess _ Nothing = sendErrorEvent mvarCtx "[setExceptionBreakpointsRequestHandler] ghci not started."
    withProcess filters (Just ghciProc) 
      | null filters                      = withCommands ghciProc ["-fno-break-on-exception", "-fno-break-on-error"]
      | filters == ["break-on-error"]     = withCommands ghciProc ["-fno-break-on-exception", "-fbreak-on-error"]
      | filters == ["break-on-exception"] = withCommands ghciProc ["-fbreak-on-exception",    "-fno-break-on-error"]
      | otherwise                         = withCommands ghciProc ["-fbreak-on-exception",    "-fbreak-on-error" ] 
  
    withCommands _ [] = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let res    = J.defaultSetExceptionBreakpointsResponse resSeq req
          resStr = J.encode res
      sendResponse mvarCtx resStr

    withCommands ghciProc (x:xs) = G.set ghciProc outHdl x >>= \case
      Right _ -> withCommands ghciProc xs
      Left err  -> do
        let msg = L.intercalate " " ["setExceptionBreakpoints request error.", show req, show err]
        resSeq <- getIncreasedResponseSequence mvarCtx
        sendResponse mvarCtx $ J.encode $ J.errorSetExceptionBreakpointsResponse resSeq req msg 
        sendErrorEvent mvarCtx msg

    outHdl = sendStdoutEvent mvarCtx

-- |
--
--
continueRequestHandler :: MVar DebugContextData -> J.ContinueRequest -> IO ()
continueRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req
  
  resSeq <- getIncreasedResponseSequence mvarCtx
  let resStr = J.encode $ J.defaultContinueResponse resSeq req
  sendResponse mvarCtx resStr

  proceedDebug mvarCtx

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["continue request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorContinueResponse resSeq req msg 
      sendErrorEvent mvarCtx msg


-- |
--
--
nextRequestHandler :: MVar DebugContextData -> J.NextRequest -> IO ()
nextRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  case debugStoppedPosDebugContextData ctx of
    Nothing -> do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let res    = J.defaultNextResponse resSeq req
          resStr = J.encode res{J.successNextResponse = False, J.messageNextResponse = "debug is initialized but not started yet. press F5(continue)."}
      sendResponse mvarCtx resStr
    Just _ -> next

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["stepOver request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorNextResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    next = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

    withProcess Nothing = sendErrorEvent mvarCtx "[nextRequestHandler] ghci not started."

    withProcess (Just ghciProc) = G.stepLocal ghciProc outHdl >>= \case
      Left err  -> do
        sendErrorEvent mvarCtx $ show err
        sendTerminateEvent mvarCtx

      Right Nothing    -> do
        resSeq <- getIncreasedResponseSequence mvarCtx
        let res    = J.defaultNextResponse resSeq req
            resStr = J.encode res
        sendResponse mvarCtx resStr

        breakByException mvarCtx

      Right (Just pos) -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}
  
        resSeq <- getIncreasedResponseSequence mvarCtx
        let res    = J.defaultNextResponse resSeq req
            resStr = J.encode res
        sendResponse mvarCtx resStr
  
        resSeq <- getIncreasedResponseSequence mvarCtx
        let stopEvt    = J.defaultStoppedEvent resSeq
            stopEvtStr = J.encode stopEvt
        sendEvent mvarCtx stopEvtStr

    outHdl = sendStdoutEvent mvarCtx


-- |
--
--
stepInRequestHandler :: MVar DebugContextData -> J.StepInRequest -> IO ()
stepInRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  case debugStoppedPosDebugContextData ctx of
    Nothing -> do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let res    = J.defaultStepInResponse resSeq req
          resStr = J.encode res{J.successStepInResponse = False, J.messageStepInResponse = "debug is initialized but not started yet. press F5(continue)."}
      sendResponse mvarCtx resStr
    Just _ -> stepIn

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["stepIn request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorStepInResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    stepIn = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

    withProcess Nothing = sendErrorEvent mvarCtx "[stepInRequestHandler] ghci not started."

    withProcess (Just ghciProc) = G.step ghciProc outHdl >>= \case
      Left err  -> do
        sendErrorEvent mvarCtx $ show err
        sendTerminateEvent mvarCtx

      Right Nothing    -> do
        resSeq <- getIncreasedResponseSequence mvarCtx
        let res    = J.defaultStepInResponse resSeq req
            resStr = J.encode res
        sendResponse mvarCtx resStr
        
        breakByException mvarCtx

      Right (Just pos) -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}
  
        resSeq <- getIncreasedResponseSequence mvarCtx
        let res    = J.defaultStepInResponse resSeq req
            resStr = J.encode res
        sendResponse mvarCtx resStr
  
        resSeq <- getIncreasedResponseSequence mvarCtx
        let stopEvt    = J.defaultStoppedEvent resSeq
            stopEvtStr = J.encode stopEvt
        sendEvent mvarCtx stopEvtStr

    outHdl = sendStdoutEvent mvarCtx


-- |
--
--
stackTraceRequestHandler :: MVar DebugContextData -> J.StackTraceRequest -> IO ()
stackTraceRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  case debugStoppedPosDebugContextData ctx of
    Nothing -> do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let body = J.StackTraceBody [] 0
          res  = J.defaultStackTraceResponse resSeq req
          resStr = J.encode $ res{J.bodyStackTraceResponse = body}
      sendResponse mvarCtx resStr
    
    Just rangeData -> do
      frames <- createStackFrames rangeData
      debugM _LOG_NAME $ show frames

      resSeq <- getIncreasedResponseSequence mvarCtx
      let body   = J.StackTraceBody (reverse frames) (length frames)
          res    = J.defaultStackTraceResponse resSeq req
          resStr = J.encode $ res{J.bodyStackTraceResponse = body}
      sendResponse mvarCtx resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["stackTrace request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorStackTraceResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    createStackFrames pos = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess pos

    withProcess pos Nothing = do
      sendErrorEvent mvarCtx "[stackTraceRequestHandler] ghci not started."
      defaultFrame pos

    withProcess pos (Just ghciProc) = G.history ghciProc outHdl >>= \case
      Left err   -> do
        sendErrorEvent mvarCtx $ show err
        defaultFrame pos

      Right dats -> do
        cwd <- workspaceDebugContextData <$> readMVar mvarCtx
        cfs <- defaultFrame pos
        foldM (convTrace2Frame cwd) cfs dats

    convTrace2Frame cwd xs (G.StackFrame traceId funcName (G.SourcePosition file sl sc el ec)) = return $ 
      J.StackFrame traceId funcName (J.Source (Just (src2mod cwd file)) file Nothing Nothing) sl sc el ec : xs

    defaultFrame (G.SourcePosition file sl sc el ec) = do
      ctx <- readMVar mvarCtx
      let cwd = workspaceDebugContextData ctx
          csf = J.StackFrame 0 "[BP]" (J.Source (Just (src2mod cwd file)) file Nothing Nothing) sl sc el ec
      return  [csf]

    outHdl = debugM _LOG_NAME


-- |
--
--
scopesRequestHandler :: MVar DebugContextData -> J.ScopesRequest -> IO ()
scopesRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let args    = J.argumentsScopesRequest req
      traceId = J.frameIdScopesArguments args

  moveFrame mvarCtx traceId

  resSeq <- getIncreasedResponseSequence mvarCtx
  let resStr = J.encode $ J.defaultScopesResponse resSeq req
  sendResponse mvarCtx resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["scopes request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorScopesResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

-- |
--
--
variablesRequestHandler :: MVar DebugContextData -> J.VariablesRequest -> IO ()
variablesRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  vals <- currentBindings

  resSeq <- getIncreasedResponseSequence mvarCtx
  let res = J.defaultVariablesResponse resSeq req
      resStr = J.encode $ res{J.bodyVariablesResponse = J.VariablesBody vals}
  sendResponse mvarCtx resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["variables request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorVariablesResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    currentBindings = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

    withProcess Nothing = do
      sendErrorEvent mvarCtx "[variablesRequestHandler] ghci not started."
      return []

    withProcess (Just ghciProc) = G.bindings ghciProc outHdl >>= \case
      Left err   -> do
        sendErrorEvent mvarCtx $ show err
        return []

      Right dats -> return $ map convBind2Vals dats
    
    convBind2Vals (G.BindingData varName modName val) = J.Variable varName modName val (Just varName) 0

    outHdl = debugM _LOG_NAME


-- |
--
--
threadsRequestHandler :: MVar DebugContextData -> J.ThreadsRequest -> IO ()
threadsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  resSeq <- getIncreasedResponseSequence mvarCtx
  let resStr = J.encode $ J.defaultThreadsResponse resSeq req
  sendResponse mvarCtx resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["threads request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorThreadsResponse resSeq req msg 
      sendErrorEvent mvarCtx msg


-- |
--
--
evaluateRequestHandler :: MVar DebugContextData -> J.EvaluateRequest -> IO ()
evaluateRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let (J.EvaluateArguments exp _ ctx) = J.argumentsEvaluateRequest req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess ctx exp

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["evaluate request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorEvaluateResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    withProcess _ _ Nothing = sendErrorEvent mvarCtx "[evaluateRequestHandler] ghci not started."

    withProcess "watch" exp (Just ghciProc) = G.showType ghciProc outHdl exp >>= \case
      Left err -> do
        errorM _LOG_NAME $ show err
        evaluateResponse err ""

      Right typeStr -> case isFunction typeStr of 
        True  -> evaluateResponse "" (getOnlyType typeStr)
        False -> G.force ghciProc outHdl exp >>= \case
          Right valStr -> evaluateResponse (getOnlyValue valStr) (getOnlyType typeStr)
          Left _ -> evaluateResponse "" (getOnlyType typeStr)

    withProcess "hover" exp (Just ghciProc) = G.showType ghciProc outHdl exp >>= \case
      Left err -> do
        errorM _LOG_NAME $ show err
        evaluateResponse err ""
      Right typeStr -> evaluateResponse typeStr (getOnlyType typeStr)

    withProcess _ exp (Just ghciProc) 
      | null (U.strip exp) = do
         evaluateResponse "" ""
         sendStdoutEvent mvarCtx (G.promptGHCiProcess ghciProc)
      | otherwise = replHandler ghciProc $ map U.rstrip (lines exp)
      
    replHandler _ [] = do
      errorM _LOG_NAME "[replHandler] invalid inputs."
      evaluateResponse "" ""
    
    replHandler ghciProc (exp:[]) 
      | isPermitCmd (U.strip exp) = G.exec ghciProc outHdl exp >>= \case
          Left err ->  do
            errorM _LOG_NAME $ show err
            evaluateResponse "" ""
            sendErrorEvent mvarCtx $ (G.promptGHCiProcess ghciProc) ++ exp ++ "\n" ++ show err
            sendStdoutEvent mvarCtx $ G.promptGHCiProcess ghciProc
          Right cmdStr -> do
            evaluateResponse "" ""
            sendStdoutEvent mvarCtx $ (G.promptGHCiProcess ghciProc) ++ exp ++ "\n" ++ cmdStr
      | otherwise = do
          evaluateResponse "" ""
          sendErrorEvent mvarCtx $ (G.promptGHCiProcess ghciProc) ++ exp ++ "\n can not use these commands.\n"  ++ show _NOT_PERMIT_REPL_COMMANDS ++ "\n"
          sendStdoutEvent mvarCtx $ G.promptGHCiProcess ghciProc

    replHandler ghciProc exps = G.exec ghciProc outHdl ":{" >>= \case
      Left err ->  do
        errorM _LOG_NAME $ show err
        evaluateResponse "" ""
        sendErrorEvent mvarCtx $ (G.promptGHCiProcess ghciProc) ++ ":{\n" ++ show err
      Right cmdStr -> replsHandler ghciProc (exps ++ [":}"]) $ (G.promptGHCiProcess ghciProc) ++ ":{\n" ++ cmdStr
    
    replsHandler _ [] acc = do
      evaluateResponse "" ""
      sendStdoutEvent mvarCtx acc

    replsHandler ghciProc (x:xs) acc 
      | isPermitCmd (U.strip x) = G.exec ghciProc outHdl x >>= \case
          Left err -> do
            evaluateResponse "" ""
            sendErrorEvent mvarCtx $ acc ++ x ++ "\n" ++ show err
            sendStdoutEvent mvarCtx $ G.promptGHCiProcess ghciProc

          Right cmdStr -> replsHandler ghciProc xs $ acc ++ x ++ "\n" ++ cmdStr
      | otherwise = do
          evaluateResponse "" ""
          sendErrorEvent mvarCtx $ acc ++ x ++ "\n can not use these commands.\n"  ++ show _NOT_PERMIT_REPL_COMMANDS ++ "\n"
          sendStdoutEvent mvarCtx $ G.promptGHCiProcess ghciProc

    isPermitCmd c = 0 == (length ( filter (flip U.startswith c) _NOT_PERMIT_REPL_COMMANDS))

    outHdl = debugM _LOG_NAME

    isFunction str = case parse isFunctionParser "isFunction" str of
      Right _ -> True
      Left _  -> False

    isFunctionParser = manyTill anyChar (string "->")

    evaluateResponse msg typeStr = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let body   = J.EvaluateBody msg typeStr 0
          res    = J.defaultEvaluateResponse resSeq req
          resStr = J.encode res{J.bodyEvaluateResponse = body}
      sendResponse mvarCtx resStr
  
    -- |
    --  force result parser
    --
    --  parser of
    --    Phoityne>>= :force x
    --    x = 8
    --    Phoityne>>=
    --
    getOnlyValue :: String -> String
    getOnlyValue str = case parse getOnlyValueParser "getOnlyValue" str of
      Right vals -> vals
      Left _ -> str
      where
        getOnlyValueParser = do
          _ <- manyTill anyChar (string " = ")
          manyTill anyChar eof

    -- |
    --  type result parser
    --
    --  parser of
    --    Phoityne>>= :type x
    --    x :: Int -> Int
    --    Phoityne>>=
    --
    getOnlyType :: String -> String
    getOnlyType str = case parse getOnlyTypeParser "getOnlyType" str of
      Right vals -> vals
      Left _ -> str
      where
        getOnlyTypeParser = do
          _ <- manyTill anyChar (string " :: ")
          manyTill anyChar eof


-- |
--
--
completionsRequestHandler :: MVar DebugContextData -> J.CompletionsRequest -> IO ()
completionsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let (J.CompletionsArguments _ key _ _) = J.argumentsCompletionsRequest req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess key

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["completions request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse mvarCtx $ J.encode $ J.errorCompletionsResponse resSeq req msg 
      sendErrorEvent mvarCtx msg

    withProcess _ Nothing = sendErrorEvent mvarCtx "[completionsRequestHandler] ghci not started."

    withProcess key (Just ghciProc) = G.complete ghciProc outHdl key 50 >>= \case
      Left err -> do
        errorM _LOG_NAME $ show err
        resSeq <- getIncreasedResponseSequence mvarCtx
        let resStr = J.encode $ J.errorCompletionsResponse resSeq req $ show err
        sendResponse mvarCtx resStr

      Right xs -> do
        resSeq <- getIncreasedResponseSequence mvarCtx
        let bd = J.CompletionsResponseBody $ map createItem xs
            res = J.defaultCompletionsResponse resSeq req 
        
        let resStr = J.encode $ res {J.bodyCompletionsResponse = bd}
        sendResponse mvarCtx resStr

    createItem (':':xs) = J.CompletionsItem xs
    createItem xs = J.CompletionsItem xs

    outHdl = debugM _LOG_NAME


-- |=====================================================================
--
--  Utility

-- |
--
--
src2mod :: FilePath -> FilePath -> String
src2mod cwd src 
  | length cwd >= length src = ""
  | otherwise = L.intercalate "."
      $ map takeBaseName
      $ reverse
      $ takeWhile startUpperCase
      $ reverse
      $ splitOneOf [_SEP_WIN, _SEP_UNIX]
      $ drop (length cwd) src

  where
    startUpperCase modName 
      | null modName = False
      | otherwise = isUpper $ head modName


-- |
--
--
getIncreasedResponseSequence :: MVar DebugContextData -> IO Int
getIncreasedResponseSequence mvarCtx = do
  ctx <- takeMVar mvarCtx
  let resSec = 1 + resSeqDebugContextData ctx
  putMVar mvarCtx ctx{resSeqDebugContextData = resSec}
  return resSec


-- |
--
--
runGHCi :: MVar DebugContextData
        -> String
        -> String
        -> M.Map String String
        -> IO (Either G.ErrorData G.GHCiProcess)
runGHCi mvarCtx cmdStr pmt envs = do
  ctx <- readMVar mvarCtx
  let cmdList = filter (not.null) $ U.split " " cmdStr
      cmd  = head cmdList
      opts = tail cmdList
      cwd  = workspaceDebugContextData ctx
  
  G.start outHdl cmd opts cwd pmt envs
  
  where
    outHdl = sendStdoutEvent mvarCtx 


-- |
--
--
loadHsFile :: MVar DebugContextData -> FilePath -> IO Bool
loadHsFile mvarCtx path = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
  Nothing -> do
    sendErrorEvent mvarCtx $ "load file fail.[" ++ path ++ "]" ++ " ghci not started."
    return False
  Just ghciProc -> G.loadFile ghciProc outHdl path >>= withFileLoadResult ghciProc
    
  where
    outHdl msg = do
      sendStdoutEvent mvarCtx msg

    withFileLoadResult _ (Left err) = do
      sendErrorEvent mvarCtx $ "load file fail.[" ++ path ++ "]" ++ " " ++ err
      return False

    withFileLoadResult ghciProc (Right mods) = G.loadModule ghciProc outHdl mods >>= \case
      Left err -> do  
        sendErrorEvent mvarCtx $ "load module fail. " ++ show mods ++ " " ++ err
        return False
      Right _ -> return True


-- |
--  ブレークポイントをGHCi上でdeleteする
--
deleteBreakPointOnGHCi :: MVar DebugContextData -> BreakPointData -> IO ()
deleteBreakPointOnGHCi mvarCtx bp@(BreakPointData _ _ (Just breakNo) _ _ _) = 
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
    Nothing -> sendErrorEvent mvarCtx $ "[deleteBreakPointOnGHCi] ghci not started. " ++ show bp
    Just ghciProc -> G.delete ghciProc outHdl breakNo >>= withResult
    
  where
    outHdl = sendStdoutEvent mvarCtx

    withResult (Left err) = sendErrorEvent mvarCtx $ "[deleteBreakPointOnGHCi] " ++ err ++ " " ++ show bp
    withResult (Right _) = return ()

deleteBreakPointOnGHCi mvarCtx bp = sendErrorEvent mvarCtx $ "[deleteBreakPointOnGHCi] invalid delete break point. "  ++ show bp

-- |
--  GHCi上でブレークポイントを追加する
--
addBreakPointOnGHCi :: MVar DebugContextData -> BreakPointData -> IO (Either String (Int, G.SourcePosition))
addBreakPointOnGHCi mvarCtx bp@(BreakPointData modName (G.SourcePosition _ lineNo col _ _) _ _ _ _) =
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
    Nothing -> do
      errorM _LOG_NAME $ "[addBreakPointOnGHCi] ghci not started. " ++ show bp
      return $ Left $ "[addBreakPointOnGHCi] ghci not started. " ++ show bp
    Just ghciProc -> G.setBreak ghciProc outHdl modName lineNo col
  
  where
    outHdl = sendStdoutEvent mvarCtx

-- |
--  GHCi上で関数ブレークポイントを追加する
--
addFunctionBreakPointOnGHCi :: MVar DebugContextData -> BreakPointData -> IO (Either String (Int, G.SourcePosition))
addFunctionBreakPointOnGHCi mvarCtx bp@(BreakPointData name _ _ _ _ _) =
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
    Nothing -> do
      errorM _LOG_NAME $ "[addFunctionBreakPointOnGHCi] ghci not started. " ++ show bp
      return $ Left $ "[addFunctionBreakPointOnGHCi] ghci not started. " ++ show bp
    Just ghciProc -> G.setFuncBreak ghciProc outHdl name
  
  where
    outHdl = sendStdoutEvent mvarCtx
    

-- |
--  Loggerのセットアップ
-- 
setupLogger :: FilePath -> Priority -> IO ()
setupLogger logFile level = do
  logStream <- openFile logFile AppendMode
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level
  
  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel level

-- |
--
-- 
watch :: MVar DebugContextData -> IO ()
watch mvarCtx = do
  _ <- forkIO $ watchFiles mvarCtx
  return ()

watchFiles :: MVar DebugContextData -> IO ()
watchFiles mvarCtx = do
  FSN.withManagerConf FSN.defaultConfig{FSN.confDebounce  = FSN.Debounce 1} $ \mgr -> do

    ctx <- readMVar mvarCtx
    let dir = workspaceDebugContextData ctx
  
    debugM _LOG_NAME $ "start watch files in [" ++ dir ++ "]"
    _ <- FSN.watchTree mgr dir hsFilter action
  
    forever $ threadDelay 1000000

  return ()
  
  where
    hsFilter event = U.endswith _HS_FILE_EXT $ FSN.eventPath event

    action event = do

      ctx <- readMVar mvarCtx
      withDebugStarted event $ debugStartedDebugContextData ctx

    withDebugStarted _ True = sendRestartEvent mvarCtx
    withDebugStarted event False = do
      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{modifiedDebugContextData = True}
      loadHsFile mvarCtx (FSN.eventPath event) >> return ()


-- |
--
--
moveFrame :: MVar DebugContextData -> Int -> IO ()
moveFrame mvarCtx traceId = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess
  where
    withProcess Nothing = sendErrorEvent mvarCtx "[moveFrame] ghci not started."
    withProcess (Just ghciProc) = do
      ctx <- readMVar mvarCtx
      let curTraceId = currentFrameIdDebugContextData ctx
          moveCount  = curTraceId - traceId
          traceCmd   = if 0 > moveCount then G.back ghciProc outHdl
                        else G.forward ghciProc outHdl

      -- _ <- traceCmd (abs moveCount)
      mapM_ traceCmd [1..(abs moveCount)]

      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{currentFrameIdDebugContextData = traceId}

    outHdl = sendStdoutEvent mvarCtx

-- |
--
--
breakByException :: MVar DebugContextData -> IO ()
breakByException mvarCtx = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess
  where
    withProcess Nothing = do
      sendErrorEvent mvarCtx "[breakByException] ghci not started."
      sendTerminateEvent mvarCtx

    withProcess (Just ghciProc) = do
      debugM _LOG_NAME $ "exception occured."
      G.exec ghciProc outHdl ":force _exception" >>= withExceptionMsg ghciProc

    withExceptionMsg _ (Left err) = do
      sendErrorEvent mvarCtx $ "[breakByException] can't get exception message. " ++ err
      sendTerminateEvent mvarCtx

    withExceptionMsg ghciProc (Right msg) = G.history ghciProc outHdl >>= \case
        Left err   -> do
          sendErrorEvent mvarCtx $ show err
          sendTerminateEvent mvarCtx

        Right [] -> do
          sendErrorEvent mvarCtx "[breakByException] invalid exception history."
          sendTerminateEvent mvarCtx

        Right ((G.StackFrame _ _ pos):_) -> do
          ctx <- takeMVar mvarCtx
          putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}
          
          resSeq <- getIncreasedResponseSequence mvarCtx
          let stopEvt    = J.defaultStoppedEvent resSeq
              stopEvtStr = J.encode stopEvt{J.bodyStoppedEvent = J.exceptionStoppedEventBody (unlines (init (lines msg)))}
          sendEvent mvarCtx stopEvtStr

    -- |
    --
    outHdl = debugM _LOG_NAME
    -- outHdl = sendStdoutEvent mvarCtx


-- |
--
--
proceedDebug :: MVar DebugContextData -> IO ()
proceedDebug mvarCtx = do
  ctx <- readMVar mvarCtx
  let started = debugStartedDebugContextData ctx
      proc    = ghciProcessDebugContextData ctx
  
  proceed started proc

  where
    -- |
    --
    proceed _ Nothing = sendErrorEvent mvarCtx "[proceedDebug] ghci not started."
    proceed True (Just ghciProc) = G.trace ghciProc outHdl >>= \case
      Left err  -> do
        -- end of debugging successfully.
        debugM _LOG_NAME $ show err
        sendTerminateEvent mvarCtx
      Right (Just pos) -> breakOrContinue pos
      Right Nothing    -> breakByException mvarCtx

    proceed False (Just ghciProc) = do
      ctx <- readMVar mvarCtx
      withModified (modifiedDebugContextData ctx) ghciProc

    -- |
    --
    withModified True _ = sendRestartEvent mvarCtx  
    withModified False ghciProc = G.traceMain ghciProc outHdl >>= \case
      Left err  -> do
        -- end of debugging successfully.
        debugM _LOG_NAME $ show err
        sendTerminateEvent mvarCtx

      Right Nothing -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{currentFrameIdDebugContextData = 0, debugStartedDebugContextData = True}
        breakByException mvarCtx

      Right (Just pos) -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{currentFrameIdDebugContextData = 0, debugStartedDebugContextData = True}
        breakOrContinue pos

    breakOrContinue pos = findBreakPointType mvarCtx pos >>= \case
      UnknownBreakPoint -> do
        sendErrorEvent mvarCtx $ "[proceedDebug] invalid break point status. " ++ show pos
        sendTerminateEvent mvarCtx

      SourceBreakPoint bp -> do
        breakOrContinueSourceBreakPoint mvarCtx bp >>= \case
          DoBreak    -> sendStopEvent pos
          DoContinue -> proceedDebug mvarCtx
          DoError m  -> do
            sendErrorEvent mvarCtx m
            sendTerminateEvent mvarCtx

      FunctionBreakPoint bp -> do
        breakOrContinueFunctionBreakPoint mvarCtx bp >>= \case
          DoBreak    -> sendStopEvent pos
          DoContinue -> proceedDebug mvarCtx
          DoError m  -> do
            sendErrorEvent mvarCtx m
            sendTerminateEvent mvarCtx

    -- |
    --
    outHdl = sendStdoutEvent mvarCtx

    -- |
    --
    sendStopEvent pos = do
      debugM _LOG_NAME $ show pos
  
      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}
  
      resSeq <- getIncreasedResponseSequence mvarCtx
      let stopEvt    = J.defaultStoppedEvent resSeq
          stopEvtStr = J.encode stopEvt
      sendEvent mvarCtx stopEvtStr


-- |
--
data BreakPointType = 
    SourceBreakPoint BreakPointData
  | FunctionBreakPoint BreakPointData
  | UnknownBreakPoint deriving (Show, Read, Eq)


-- |
--
data BreakOrContinueType = DoBreak | DoContinue | DoError String deriving (Show, Read, Eq)


-- |
--
findBreakPointType :: MVar DebugContextData -> G.SourcePosition -> IO BreakPointType
findBreakPointType mvarCtx pos = do
  ctx <- readMVar mvarCtx

  let bpMap = breakPointDatasDebugContextData ctx
      funcMap = functionBreakPointDatasDebugContextData ctx

  case M.lookup pos bpMap of
    Just bp -> return $ SourceBreakPoint bp
    Nothing -> case M.lookup pos funcMap of
      Just bp -> return $ FunctionBreakPoint bp
      Nothing -> return UnknownBreakPoint


-- |
--
breakOrContinueSourceBreakPoint :: MVar DebugContextData -> BreakPointData -> IO BreakOrContinueType
breakOrContinueSourceBreakPoint mvarCtx bp = do
  ctx <- takeMVar mvarCtx
  let bpMap = breakPointDatasDebugContextData ctx
      newMap = M.adjust incrementBreakPointHitCount (getBreakPointKey bp) bpMap
  putMVar mvarCtx ctx{breakPointDatasDebugContextData = newMap}

  breakOrContinueByCondition mvarCtx $ incrementBreakPointHitCount bp


-- |
--
breakOrContinueFunctionBreakPoint :: MVar DebugContextData -> BreakPointData -> IO BreakOrContinueType
breakOrContinueFunctionBreakPoint mvarCtx bp = do
  ctx <- takeMVar mvarCtx
  let bpMap = functionBreakPointDatasDebugContextData ctx
      newMap = M.adjust incrementBreakPointHitCount (getBreakPointKey bp) bpMap
  putMVar mvarCtx ctx{functionBreakPointDatasDebugContextData = newMap}
 
  breakOrContinueByCondition mvarCtx $ incrementBreakPointHitCount bp

-- |
--
breakOrContinueByCondition :: MVar DebugContextData -> BreakPointData -> IO BreakOrContinueType
breakOrContinueByCondition _ (BreakPointData _ _ _ Nothing Nothing _) = return DoBreak
breakOrContinueByCondition _ (BreakPointData _ _ _ Nothing (Just hitCond) hitCount) = breakOrContinueByHitCount hitCond hitCount
breakOrContinueByCondition mvarCtx (BreakPointData _ _ _ (Just condStr) Nothing _) = breakOrContinueByOpCond mvarCtx condStr
breakOrContinueByCondition mvarCtx (BreakPointData _ _ _ (Just condStr) (Just hitCond) hitCount) =
  breakOrContinueByHitCount hitCond hitCount >>= \case
    DoError m  -> return $ DoError m
    DoContinue -> return DoContinue
    DoBreak    -> breakOrContinueByOpCond mvarCtx condStr

-- |
--
breakOrContinueByOpCond :: MVar DebugContextData -> String -> IO BreakOrContinueType
breakOrContinueByOpCond mvarCtx condStr = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess
  where
    -- |
    --
    withProcess Nothing = return $ DoError "[breakOrContinueByOpCond] ghci not started."
    withProcess (Just ghciProc) = do
      forceBindings ghciProc outHdl
      G.execBool ghciProc outHdl condStr >>= \case
        Left err -> return . DoError $ "[breakOrContinueByCondition] " ++ err ++ ". '" ++ condStr ++ "'"
        Right False -> do
          debugM _LOG_NAME $ "continued because condition False. " ++ condStr
          return DoContinue
        Right True -> do
          debugM _LOG_NAME $ "stopped because condition not False. "  ++ condStr
          return DoBreak

    -- |
    --
    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      debugM _LOG_NAME msg

    -- |
    --
    forceBindings ghciProc outHdl = G.bindings ghciProc outHdl >>= \case
      Left err -> sendErrorEvent mvarCtx $ "[forceBindings] " ++ err
      Right bs -> mapM_ (forceBind ghciProc outHdl . G.nameBindingData) bs

    forceBind ghciProc outHdl name = G.force ghciProc outHdl name >>= \case
      Left err -> sendErrorEvent mvarCtx $ "[forceBindings] " ++ err
      Right _  -> return ()


-- |
--
breakOrContinueByHitCount :: String -> Int -> IO BreakOrContinueType
breakOrContinueByHitCount hitCondStr hitCount = case parse hitCondParser "Hit count condition parser" (U.strip hitCondStr) of
  Left  msg  -> return . DoError $ show msg  ++ ". '" ++ hitCondStr ++ "'"
  Right func -> if func hitCount
    then do
      debugM _LOG_NAME $ "stopped because satisfy hit count. " ++ hitCondStr ++ ", " ++ show hitCount
      return DoBreak
    else do
      debugM _LOG_NAME $ "continued because not satisfy hit count. " ++ hitCondStr ++ ", " ++ show hitCount
      return DoContinue

  where
    -- |
    --
    hitCondParser = try digitCondParser <|> opCondParser

    -- |
    --
    digitCondParser = do
      valStr <- manyTill digit eof
      return $ (<=) (read valStr)

    -- |
    --
    opCondParser = do
      op <- manyTill anyChar (space <|> lookAhead digit)
      many space
      valStr <- manyTill digit eof
      let val = read valStr 
      case op of
        "=="  -> return $ (==) val
        "/="  -> return $ (/=) val
        "<"   -> return $ flip (<) val
        ">"   -> return $ flip (>)  val
        "<="  -> return $ flip (<=) val
        ">="  -> return $ flip (>=) val
        "%"   -> return $ modInternal val
        "mod" -> return $ modInternal val
        other -> fail other

    -- |
    --
    modInternal :: Int -> Int -> Bool
    modInternal a b = mod b a == 0

