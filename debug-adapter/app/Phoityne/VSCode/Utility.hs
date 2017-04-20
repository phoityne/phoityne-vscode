
module Phoityne.VSCode.Utility where

import Data.Either.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ConfigFile as CFG
import qualified Data.Tree as TR


-- |
--
str2bs :: String -> BS.ByteString
str2bs = TE.encodeUtf8 . T.pack

-- |
--
bs2str :: BS.ByteString -> String
bs2str = T.unpack. TE.decodeUtf8

-- |
--
str2lbs :: String -> LBS.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack

-- |
--
lbs2str :: LBS.ByteString -> String
lbs2str = TL.unpack. TLE.decodeUtf8

-- |
--
getIniItems :: CFG.ConfigParser
            -> CFG.SectionSpec
            -> [(CFG.OptionSpec, String)]
getIniItems cp sec = foldr go [] opts
  where
    opts = forceEither $ CFG.options cp sec
    go opt acc = let value = forceEither $ CFG.get cp sec opt in
                 (opt, value) : acc


-- |
-- 
addChildTree :: TR.Tree a -> TR.Tree a -> TR.Tree a
addChildTree parent child = parent { TR.subForest = child : curForest}
  where curForest = TR.subForest parent


-- |
-- 
pushWithLimit :: [a] -> a -> Int -> [a]
pushWithLimit [] item _ = [item]
pushWithLimit buf item maxSize = if length buf > maxSize then item : L.init buf else item : buf

-- |
--
rdrop :: Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse

