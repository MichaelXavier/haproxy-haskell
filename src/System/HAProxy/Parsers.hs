{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HAProxy.Parsers ( Deserializeable(..)
                              , parseBS
                              , parseText
                              ) where

import BasicPrelude hiding (lookup)
import Control.Applicative
import Data.HashMap.Strict (lookup)
import Data.Attoparsec.Text
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.Thyme.Calendar (Day, gregorian, YearMonthDay(..))
import Data.Thyme.Clock (NominalDiffTime, fromSeconds)
import qualified Data.Text as T

import System.HAProxy.Types
import System.HAProxy.Util

import Debug.Trace

class Deserializeable a where
  parser :: Parser a

instance Deserializeable ShowInfoResp where
  parser = e2err =<< convertPairs . HM.fromList <$> parsePairs
    where
      parsePairs :: Parser [(Text, Text)]
      parsePairs = parsePair `sepBy` endOfLine

      parsePair = liftA2 (,) parseKey parseValue
      parseKey   = takeTill (==':') <* kvSep

      kvSep      = string ": "

      parseValue = takeTill isEndOfLine

      convertPairs hm = traceShow hm $
        ShowInfoResp <$> lu "Name"
                     <*> lu "Version"
                     <*> (parseDay =<< lu "Release_date")
                     <*> parseInt' "Nbproc"
                     <*> parseInt' "Process_num"
                     <*> lu "Pid"
                     <*> (parseNDT =<< lu "Uptime_sec")
                     <*> (parseMS  =<< lu "Memmax_MB")
                     <*> parseInt' "Ulimit-n"
                     <*> parseInt' "Maxsock"
                     <*> parseInt' "Maxconn"
                     <*> parseInt' "Maxpipes"
                     <*> parseInt' "CurrConns"
                     <*> parseInt' "PipesUsed"
                     <*> parseInt' "PipesFree"
                     <*> parseInt' "Tasks"
                     <*> parseInt' "Run_queue"
                     <*> lu "node"
                     <*> lu "description"
        where lu k = m2e ("Missing key " ++ k) $ lookup k hm
              parseInt' k = m2e ("Parse int failed on key " ++ k) . parseInt =<< lu k

parseBS :: Deserializeable a => ByteString -> Either String a
parseBS = parseText . decodeUtf8

parseText :: Deserializeable a => Text -> Either String a
parseText = eitherResult . parse parser

parseDay :: Text -> Either Text Day
parseDay t = 
  case T.splitOn "/" t of
    (y:m:d:[]) -> m2e "bad date format" $ convert <$> readMay y
                                          <*> readMay m
                                          <*> readMay d
    _          -> Left "bad date format"
  where convert y m d = YearMonthDay y m d ^. from gregorian

parseNDT :: Text -> Either Text NominalDiffTime
parseNDT = m2e "NDT parse fail" . fmap fromSeconds . (readMay :: Text -> Maybe Int)

parseMS :: Text -> Either Text MemorySize
parseMS = m2e "MB parse fail" . fmap MB . readMay

parseInt :: Text -> Maybe Int
parseInt = readMay

-- l2err :: (Errable m, Monad m) => Either Text a -> m a
-- l2err (Right x) = return x
-- l2err (Left e)  = raiseErr er
--   where er = Err (Just . D.text . T.unpack $ e) mempty mempty

-- probably just don't use text then
e2err :: (Monad m) => Either Text a -> m a
e2err = either (fail . T.unpack) return
