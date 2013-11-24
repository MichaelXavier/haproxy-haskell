{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module System.HAProxy ( module System.HAProxy.Types
                      , showInfo
                      ) where

import BasicPrelude
import Control.Monad.Free (liftF)
import Control.Monad.Reader
import System.HAProxy.Commands
import System.HAProxy.Types

showInfo :: (MonadReader HAConfig m, MonadIO m) => m (HAResp ShowInfoResp)
showInfo = runHAProxyCommand cmdShowInfo
