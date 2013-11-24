{-# LANGUAGE NoImplicitPrelude #-}
module System.HAProxy.Types where

import BasicPrelude
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock (NominalDiffTime)

data ShowInfo = ShowInfo deriving (Show)
data ClearCounters = ClearCounters deriving (Show)
data ClearCountersAll = ClearCountersAll deriving (Show)

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

data HAConfig = HAConfig {
  socketPath :: FilePath
} deriving (Show, Eq)

data HAError = HAError String deriving (Show, Eq) -- TODO: more defined types

type HAResp a = Either HAError a
