{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module System.HAProxy ( module System.HAProxy.Types
                      , showInfo
                      , clearCounters
                      , clearCountersAll
                      ) where

import BasicPrelude hiding (show)
import Prelude (show)
import Control.Monad.Reader
import Filesystem.Path.CurrentOS (encodeString)
import Pipes (runEffect, (>->), Producer, Producer')
import Pipes.Lift (evalStateP)
import qualified Pipes.Prelude as PP
import Network.Socket ( close
                      , connect
                      , socket
                      , Family(AF_UNIX)
                      , SocketType(Stream)
                      , defaultProtocol
                      , SockAddr(SockAddrUnix))
import qualified Pipes.Attoparsec as PA
import Pipes.ByteString (fromLazy)
import Pipes.Network.TCP hiding (connect)
import System.HAProxy.Serializers
import System.HAProxy.Parsers
import System.HAProxy.Types


clearCounters :: (MonadReader HAConfig m, MonadIO m) => m (HAResp ())
clearCounters = runHAProxyReq ClearCounters

clearCountersAll :: (MonadReader HAConfig m, MonadIO m) => m (HAResp ())
clearCountersAll = runHAProxyReq ClearCountersAll

disableServer :: (MonadReader HAConfig m, MonadIO m)
                 => ServerRef
                 ->  m (HAResp ())
disableServer = runHAProxyReq . DisableServer


enableServer :: (MonadReader HAConfig m, MonadIO m)
                 => ServerRef
                 ->  m (HAResp ())
enableServer = runHAProxyReq . EnableServer

-- getWeight :: (MonadReader HAConfig m, MonadIO m)
--              => ServerRef
--              ->  m (HAResp ())
-- getWeight = runHAProxyReq . GetWeight

setWeight :: (MonadReader HAConfig m, MonadIO m)
             => ServerRef
             -> WeightRef
             ->  m (HAResp ())
setWeight s = runHAProxyReq . GetWeight s


showErrors :: (MonadReader HAConfig m, MonadIO m)
            -> m (HAResp ShowErrorsResp)
showErrors = runHAProxyReq ShowErrors

showErrorsForProxy :: (MonadReader HAConfig m, MonadIO m)
                   -> ProxyID
                   => m (HAResp ShowErrorsResp)
showErrorsForProxy = runHAProxyReq . ShowErrorsForProxy

showInfo :: (MonadReader HAConfig m, MonadIO m)
            => m (HAResp ShowInfoResp)
showInfo = runHAProxyReq ShowInfo

showSessions :: (MonadReader HAConfig m, MonadIO m)
             => m (HAResp [Session])
showSessions = runHAProxyReq ShowSessions

showSession :: (MonadReader HAConfig m, MonadIO m)
             -> SessionID
             => m (HAResp (Maybe Session))
showSession = runHAProxyReq ShowSessions

runHAProxyReq :: forall a (m :: * -> *) b.
                 ( MonadIO m
                 , MonadReader HAConfig m
                 , Serializable a
                 , Deserializeable b )
                 => a
                 -> m (HAResp b)
runHAProxyReq req = do
  path <- asks socketPath
  liftIO $ withSocketsDo $ do
    sock <- openSocket path
    roundtrip sock `finally` close sock
  where roundtrip sock = runEffect $ runReq req (fromSocket sock 4096 >-> PP.map decodeUtf8) >-> toSocket sock

runReq :: forall (m :: * -> *) a b r.
          ( Monad m
          , Serializable a
          , Deserializeable b )
          => a
          -> Producer Text m r
          -> Producer' ByteString m (HAResp b)
runReq req pipe = evalStateP pipe $ do
  sendReq req
  wrapResp <$> parseResp
  where wrapError   = Left . HAError . show
        wrapSuccess = Right . snd
        wrapResp    = either wrapError wrapSuccess
        sendReq     = fromLazy . terminateCmd . serialize
        parseResp   = lift (PA.parse parser)

terminateCmd :: LByteString -> LByteString
terminateCmd = (<> ";")

openSocket :: FilePath -> IO Socket
openSocket p = do sock <- socket AF_UNIX Stream defaultProtocol
                  connect sock sockAddr
                  return sock
  where sockAddr = SockAddrUnix . encodeString $ p
