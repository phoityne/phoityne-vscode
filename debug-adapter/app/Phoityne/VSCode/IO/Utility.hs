{-# LANGUAGE MultiWayIf #-}

module Phoityne.VSCode.IO.Utility where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import GHC.IO.Encoding
import Distribution.System
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

-- |
--  
-- 
getReadHandleEncoding :: IO TextEncoding
getReadHandleEncoding = if
  | Windows == buildOS -> mkTextEncoding "CP932//TRANSLIT"
  | otherwise -> mkTextEncoding "UTF-8//TRANSLIT"

-- |
--  
-- 
loadFile :: FilePath -> IO B.ByteString
loadFile path = do
  bs <- runResourceT
      $ C.sourceFile path
      C.$$ C.consume
  return $ B.concat bs

-- |
--  
-- 
saveFile :: FilePath -> B.ByteString -> IO ()
saveFile path cont = saveFileLBS path $ LBS.fromStrict cont

-- |
--  
-- 
saveFileLBS :: FilePath -> LBS.ByteString -> IO ()
saveFileLBS path cont = runResourceT
  $ C.sourceLbs cont
  C.$$ C.sinkFile path



