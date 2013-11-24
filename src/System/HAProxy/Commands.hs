{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module System.HAProxy.Commands ( CommandF(..)
                               , Command
                               , cmdShowInfo
                               , cmdClearCounters
                               , runHAProxyCommand ) where

import BasicPrelude hiding (show)
import Prelude (show)
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State (StateT)
import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Lift (evalStateP)
import qualified Pipes.Attoparsec as PA
import Filesystem.Path.CurrentOS (encodeString)
import Network.Socket
import System.HAProxy.Types
import Pipes.ByteString (fromLazy)
import Pipes.Network.TCP hiding (connect)

import System.HAProxy.Parsers
import System.HAProxy.Serializers

data CommandF next =
  CmdShowInfo ShowInfo (HAResp ShowInfoResp -> next) |
  CmdClearCounters ClearCounters (HAResp () -> next) deriving (Functor)

type Command = Free CommandF

-- Command Subset
cmdShowInfo :: Command (HAResp ShowInfoResp)
cmdShowInfo = liftF (CmdShowInfo ShowInfo id)

cmdClearCounters :: Command (HAResp ())
cmdClearCounters = liftF (CmdClearCounters ClearCounters id)

-- it says:
-- src/System/HAProxy/Commands.hs|47 col 1 warning| Top-level binding with no type signature:
-- ||   interpret1 :: forall a x' x (m :: * -> *) r.
-- ||                 Monad m =>
-- ||                 Free CommandF a
-- ||                 -> Proxy x' x () ByteString (StateT (Producer Text m r) m) a

-- so i'm like
-- interpret1 :: Monad m =>
--               Command a
--               -> Proxy x' x () ByteString (StateT (Producer Text m r) m) a
-- and then its like
-- src/System/HAProxy/Commands.hs|61 col 11 error| Couldn't match type `StateT (Producer Text m1 r1) m1'
-- ||               with `StateT (Producer Text m0 r0) m0'
-- || Expected type: Either HAError b0
-- ||                -> Proxy x'0 x0 () ByteString (StateT (Producer Text m0 r0) m0) a0
-- ||   Actual type: Either HAError b0
-- ||                -> Proxy x'0 x0 () ByteString (StateT (Producer Text m1 r1) m1) a0
-- || The first argument of ($) takes one argument,
-- || but its type `Either HAError b0
-- ||               -> Proxy x'0 x0 () ByteString (StateT (Producer Text m1 r1) m1) a0'
-- || has only one
-- || In a stmt of a 'do' block:
-- ||   interpret1 . rest $ either wrapError wrapSuccess result
-- || In the expression:
-- ||   do { fromLazy . terminateCmd . serialize $ cmd;
-- ||        result <- lift (parse parser);
-- ||        interpret1 . rest $ either wrapError wrapSuccess result }
interpret1 (Pure r)                      = return r
interpret1 (Free (CmdShowInfo c k))      = runCommand c k
-- interpret1 (Free (CmdClearCounters c k)) = runCommand c k
  where runCommand cmd rest = do
          fromLazy . terminateCmd . serialize $ cmd
          result <- lift (PA.parse parser)
          interpret1 . rest $ either wrapError wrapSuccess result
          where wrapError   = Left . HAError . show
                wrapSuccess = Right . snd

-- interpret2 :: ( MonadIO m )
--               => Command a
--               -> Producer Text m ()
--               -> Producer ByteString m a
interpret2 cmd p = evalStateP p (interpret1 cmd)


terminateCmd :: LByteString -> LByteString
terminateCmd = (<> ";")

-- TODO I've been told thid decodeUtf8 bit is a Very Bad Idea. I should be
-- using pipes-text when it comes out because messages > 4096 bytes are
-- probably going to mangle on word boundaries or something.
-- runHAProxyCommand :: (MonadReader HAConfig m, MonadIO m) => Command a -> m (HAResp a)
-- also it sometimes fails to parse and gets no data. I thinkwe aren't guaranteeing that we send the whole message before we get a response

-- runHAProxyCommand :: ( MonadIO m
--                      , Deserializeable a
--                      , MonadReader HAConfig m )
--                      => Command a
--                      -> m a
runHAProxyCommand cmd = do
  path <- asks socketPath
  liftIO $ withSocketsDo $ do
    sock <- openSocket path
    roundtrip sock `finally` close sock
  where roundtrip sock = runEffect $ interpret2 cmd (fromSocket sock 4096 >-> PP.map decodeUtf8) >-> toSocket sock

openSocket :: FilePath -> IO Socket
openSocket p = do sock <- socket AF_UNIX Stream defaultProtocol
                  connect sock sockAddr
                  return sock
  where sockAddr = SockAddrUnix . encodeString $ p
