{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HAProxy.Types where

import BasicPrelude hiding (lookup)
import Data.HashMap.Strict (lookup)
import Control.Lens
import qualified Data.HashMap.Strict as HM
import qualified Text.PrettyPrint.ANSI.Leijen as D
import Data.Thyme.Calendar (Day, gregorian, YearMonthDay(..))
import Data.Thyme.Clock (NominalDiffTime, fromSeconds)
import qualified Data.Text as T
import Text.Trifecta

data ShowInfo = ShowInfo deriving (Show)

newtype MemorySize = MB Int deriving (Show, Ord, Eq)

data ShowInfoResp = ShowInfoResp {
  iName         :: Text
, iVersion      :: Text
, iReleaseDate  :: Day
, iNbproc       :: Int
, iProcessNum   :: Int
, iPid          :: Text
, iUptimeSec    :: NominalDiffTime
, iMaxMemory    :: MemorySize
, iUlimitN      :: Int
, iMaxSockets   :: Int
, iMaxConns     :: Int
, iMaxPipes     :: Int
, iCurrentConns :: Int
, iPipesUsed    :: Int
, iPipesFree    :: Int
, iTasks        :: Int
, iRunQueue     :: Int
, iNode         :: Text
, iDescription  :: Text
} deriving (Show, Eq)
-- Name: HAProxy
-- Version: 1.4.24
-- Release_date: 2013/06/17
-- Nbproc: 1
-- Process_num: 1
-- Pid: 11142
-- Uptime: 0d 0h00m36s
-- Uptime_sec: 36
-- Memmax_MB: 0
-- Ulimit-n: 8222
-- Maxsock: 8222
-- Maxconn: 4096
-- Maxpipes: 0
-- CurrConns: 1
-- PipesUsed: 0
-- PipesFree: 0
-- Tasks: 14
-- Run_queue: 1
-- node: Apollo-Minor
-- description:

class Serializable a where
  serialize :: a -> ByteString

instance Serializable ShowInfo where
  serialize = const "show info"

class Deserializeable a where
  parser :: Parser a

instance Deserializeable ShowInfoResp where
  parser = l2err =<< convertPairs . HM.fromList <$> parsePairs
    where parsePairs :: Parser [(Text, Text)]
          parsePairs = some parsePair
          parsePair = (,) <$> parseKey <*> parseValue
          parseKey   = T.pack <$> (many keyChars <* (token . char $ ':'))
          -- parseKey   = T.pack <$> many keyChars
          keyChars   = letter <|> oneOf "-_"
          parseValue = T.pack <$> (token (many . notChar $ '\n'))
          convertPairs :: HM.HashMap Text Text -> Either Text ShowInfoResp
          convertPairs hm =
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

l2err :: (Errable m, Monad m) => Either Text a -> m a
l2err (Right x) = return x
l2err (Left e)  = raiseErr er
  where er = Err (Just . D.text . T.unpack $ e) mempty mempty

m2e :: b -> Maybe a -> Either b a
m2e l = maybe (Left l) Right

parseDay :: Text -> Either Text Day
parseDay t = 
  case T.splitOn "/" t of
    (y:m:d:[]) -> m2e "bad date format" $ convert <$> readMay y
                                          <*> readMay m
                                          <*> readMay d
    _          -> Left "bad date format"
  where convert y m d = YearMonthDay y m d ^. from gregorian

parseNDT :: Text -> Either Text NominalDiffTime
parseNDT = m2e "NDT parse fail" . fmap fromSeconds . readMay

parseMS :: Text -> Either Text MemorySize
parseMS = m2e "MB parse fail" . fmap MB . readMay

parseInt :: Text -> Maybe Int
parseInt = readMay
