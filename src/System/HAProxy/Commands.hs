{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module System.HAProxy.Commands ( CommandF(..)
                               , Command
                               , cmdShowInfo
                               , runHAProxyCommand ) where

import BasicPrelude hiding (show)
import Prelude (show)
import Control.Monad.Free
import Control.Monad.Reader
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
  CmdShowInfo ShowInfo (HAResp ShowInfoResp -> next) deriving (Functor)

type Command = Free CommandF

-- Command Subset
cmdShowInfo :: Command (HAResp ShowInfoResp)
cmdShowInfo = liftF (CmdShowInfo ShowInfo id)

-- interpret1 :: (Monad m) => Command r -> m r
interpret1 (Pure r)                 = return r
interpret1 (Free (CmdShowInfo c k)) = runCommand c k

-- interpret2 :: (Monad m) => Command r -> Producer ByteString m () -> Producer ByteString m r
interpret2 cmd p = evalStateP p (interpret1 cmd)

-- runCommand :: (Monad m) => ShowInfo -> (ShowInfoResp -> Command r) -> m r
runCommand cmd rest = do
  fromLazy . serialize $ cmd
  result <- lift (PA.parse parser)
  interpret1 . rest $ either wrapError wrapSuccess result
  where wrapError   = Left . HAError . show
        wrapSuccess = Right . snd

-- runHAProxyCommand :: (MonadReader HAConfig m, MonadIO m) => Command a -> m (HAResp a)
runHAProxyCommand cmd = do
  path <- asks socketPath
  liftIO $ withSocketsDo $ do
    sock <- openSocket path
    runEffect $ interpret2 cmd (fromSocket sock 4096 >-> PP.map decodeUtf8) >-> toSocket sock

openSocket :: FilePath -> IO Socket
openSocket p = do sock <- socket AF_UNIX Stream defaultProtocol
                  connect sock sockAddr
                  return sock
  where sockAddr = SockAddrUnix . encodeString $ p
