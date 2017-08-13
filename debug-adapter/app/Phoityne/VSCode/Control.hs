{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Phoityne.VSCode.Control where

import Phoityne.VSCode.Constant
import Phoityne.VSCode.Utility
import qualified Phoityne.VSCode.Argument as A
import qualified Phoityne.VSCode.Core as GUI
import qualified Data.ByteString.Lazy as BSL

import System.IO
import Control.Concurrent
import Text.Parsec
import qualified Data.ConfigFile as C
import qualified System.Log.Logger as L

-- |
-- 
run :: A.ArgData
    -> C.ConfigParser
    -> IO Int
run _ _ = do

  hSetBuffering stdin NoBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout NoBuffering
  hSetEncoding  stdout utf8

  mvarDat <- newMVar GUI.defaultDebugContextData {GUI.responseHandlerDebugContextData = sendResponse}

  wait mvarDat

  return 1

-- |
--
-- 
wait :: MVar GUI.DebugContextData -> IO ()
wait mvarDat = go BSL.empty
  where
    go buf = BSL.hGet stdin 1 >>= withC buf
    
    withC buf c
      | c == BSL.empty = unexpectedEOF
      | otherwise      = withBuf $ BSL.append buf c
    
    withBuf buf = case parse parser "readContentLength" (lbs2str buf) of
        Left _    -> go buf
        Right len -> BSL.hGet stdin len >>= withCnt buf

    withCnt buf cnt
      | cnt == BSL.empty = unexpectedEOF
      | otherwise        = do
        GUI.handleRequest mvarDat buf cnt
        wait mvarDat

    parser = do
      string "Content-Length: "
      len <- manyTill digit (string _TWO_CRLF)
      return . read $ len

    unexpectedEOF = do
      L.criticalM _LOG_NAME "unexpected EOF on stdin."
      return ()



-- |
--
sendResponse :: BSL.ByteString -> IO ()
sendResponse str = do
  BSL.hPut stdout $ BSL.append "Content-Length: " $ str2lbs $ show (BSL.length str)
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout

