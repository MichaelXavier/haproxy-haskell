{-# LANGUAGE NoImplicitPrelude #-}
module System.HAProxy.Types where

import BasicPrelude
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock (NominalDiffTime)


data Backend = BackendName { backendName :: Text } |
               BackendID { backendID :: Int } deriving (Show)
data Server = ServerName { serverName :: Text } |
              ServerID { serverID :: Int } deriving (Show)
newtype ProxyID = ProxyID { proxyID :: Int } deriving (Show) -- maybe?
newtype SessionID = SessionID { sessionID :: Int } deriving (Show) -- maybe?

data ServerRef = ServerRef { server  :: Server
                           , backend :: Backend} deriving (Show)

data WeightRef = AbsoluteWeight Int |
                 WeightPerc Int deriving (Show)

newtype FrontendName = FrontendName { unFrontendName :: Text } deriving (Show)
newtype FrontendNumber = FrontendNumber { unFrontendNumber :: Int } deriving (Show)
data FrontendRef = FrontendByName { frontendByName :: FrontendName } |
                   FrontendByNumber { frontendByNumber :: FrontendNumber } deriving (Show)

data ConnectionRateLimit = ConnectionsPerSecond Int |
                           UnlimitedConnections deriving (Show)

data CompressionRateLimit = KBps Int |
                            UnlimitedCompression deriving (Show)

--TODO: hide these in commands
data ClearCounters = ClearCounters deriving (Show)
data ClearCountersAll = ClearCountersAll deriving (Show)
data DisableServer = DisableServer ServerRef  deriving (Show)
data EnableServer = EnableServer ServerRef  deriving (Show)
data GetWeight = GetWeight ServerRef  deriving (Show)
data SetWeight = SetWeight ServerRef WeightRef  deriving (Show)
data ShowErrors = ShowAllErrors deriving (Show)
data ShowErrorsForProxy = ShowErrorsForProxy ProxyID deriving (Show)
data ShowInfo = ShowInfo  deriving (Show)
data ShowSessions = ShowSessions  deriving (Show)
data ShowSession = ShowSession SessionID  deriving (Show)
-- data ShowStat = ShowStat   deriving (Show) -- TODO: more forms

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
