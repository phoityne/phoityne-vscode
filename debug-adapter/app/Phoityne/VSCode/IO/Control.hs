{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Phoityne.VSCode.IO.Control where

import Phoityne.VSCode.Utility
import qualified Phoityne.VSCode.Argument as A
import qualified Phoityne.VSCode.IO.Core as GUI
import qualified Data.ByteString.Lazy as BSL

import System.IO
import Control.Concurrent
import qualified Data.ConfigFile as C
import Text.Parsec

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
    go :: BSL.ByteString -> IO ()
    go buf = do
      c <- BSL.hGet stdin 1
      let newBuf = BSL.append buf c
      case readContentLength (lbs2str newBuf) of
        Left _ -> go newBuf
        Right len -> do
          cnt <- BSL.hGet stdin len
          GUI.handleRequest mvarDat newBuf cnt
          wait mvarDat
    
      where
        readContentLength :: String -> Either ParseError Int
        readContentLength = parse parser "readContentLength"
    
        parser = do
          string "Content-Length: "
          len <- manyTill digit (string _TWO_CRLF)
          return . read $ len

-- |
--
sendResponse :: BSL.ByteString -> IO ()
sendResponse str = do
  BSL.hPut stdout $ BSL.append "Content-Length: " $ str2lbs $ show (BSL.length str)
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout

-- |
--
--
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"

