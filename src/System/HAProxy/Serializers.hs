{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HAProxy.Serializers (Serializable(..)) where

import BasicPrelude

import System.HAProxy.Types
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

class Serializable a where
  serialize :: a -> LByteString

instance Serializable Backend where
  serialize (BackendName n) = t2lbs n
  serialize (BackendID n)   = "#" <> showLBS n

instance Serializable Server where
  serialize (ServerName n) = t2lbs n
  serialize (ServerID n)   = "#" <> showLBS n

instance Serializable ServerRef where
  serialize sr = mconcat [ serialize . backend $ sr
                         , "/"
                         , serialize . server $ sr]

instance Serializable WeightRef where
  serialize (AbsoluteWeight n) = showLBS n
  serialize (WeightPerc p)     = showLBS p <> "%"

-- not entirely sure if this is correct or if we need #. docs don't say
instance Serializable ProxyID where
  serialize (ProxyID i) = showLBS i

instance Serializable SessionID where
  serialize (SessionID i) = showLBS i

instance Serializable ClearCounters where
  serialize = const "clear counters"

instance Serializable ClearCountersAll where
  serialize = const "clear counters all"

instance Serializable DisableServer where
  serialize (DisableServer sr) = "disable server " <> serialize sr

instance Serializable EnableServer where
  serialize (EnableServer sr) = "enable server " <> serialize sr

instance Serializable GetWeight where
  serialize (GetWeight sr) = "get weight " <> serialize sr

instance Serializable SetWeight where
  serialize (SetWeight sr wr) = "set weight " <> serialize sr <> " " <> serialize wr

instance Serializable ShowErrors where
  serialize = const "show errors"

instance Serializable ShowErrorsForProxy where
  serialize (ShowErrorsForProxy pid) = "show errors " <> serialize pid

instance Serializable ShowInfo where
  serialize = const "show info"

instance Serializable ShowSessions where
  serialize = const "show sess"

instance Serializable ShowSession where
  serialize (ShowSession sid) = "show sess " <> serialize sid

t2lbs :: Text -> LByteString
t2lbs = LTE.encodeUtf8 . LT.fromStrict

showLBS :: (Show a) => a -> LByteString 
showLBS = t2lbs . show
