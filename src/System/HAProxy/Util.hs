{-# LANGUAGE NoImplicitPrelude #-}
module System.HAProxy.Util (m2e) where

import BasicPrelude

m2e :: b -> Maybe a -> Either b a
m2e l = maybe (Left l) Right
