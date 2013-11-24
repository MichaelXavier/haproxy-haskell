{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.HAProxy.Serializers (Serializable(..)) where

import BasicPrelude
import System.HAProxy.Types

class Serializable a where
  serialize :: a -> LByteString

instance Serializable ShowInfo where
  serialize = const "show info"

instance Serializable ClearCounters where
  serialize = const "clear counters"

instance Serializable ClearCountersAll where
  serialize = const "clear counters all"
