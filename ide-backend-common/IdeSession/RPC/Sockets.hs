{-# LANGUAGE CPP, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module provies an interface for using sockets in RPC communication.
module IdeSession.RPC.Sockets (
    portToString
  , makeSocket
  , connectToPort
  , stringToPort
  , acceptHandle
  , ReadChannel(..)
  , WriteChannel(..)
) where

import System.IO (Handle)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary
import Network
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Base64.Lazy as Base64
import Network.Socket hiding (close, sClose, accept, socketPort)

#ifdef VERSION_unix
-- import Control.Exception
-- import Control.Concurrent.MVar
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import System.Directory
#endif

newtype ReadChannel = ReadChannel PortID deriving (Generic, Typeable)
newtype WriteChannel = WriteChannel PortID deriving (Generic, Typeable)

instance Binary ReadChannel
instance Binary WriteChannel

-- Creating a socket, possibly using the given file path for temporary
-- files (i.e. when using Unix domain sockets)
makeSocket :: FilePath -> IO Socket

#ifdef VERSION_unix
makeSocket sessionDir = do
  (rpcFile, handle) <- openTempFile sessionDir "rpc"
  hClose handle
  removeFile rpcFile
  s <- listenOn $ UnixSocket rpcFile
  p <- socketPort s
  return s

-- {- On Linux we can autobind a Unix domain socket -}
-- -- mostly copied from Network.hs
-- makeSocket =
--   bracketOnError
--       (socket AF_UNIX Stream 0)
--       (sClose)
--       (\sock@(MkSocket _ _ _ _ socketStatus) -> do
--           setSocketOption sock ReuseAddr 1
--           -- this line provides auto-binding of the socket
--           setSocketOption sock passCredOpt 1
--           modifyMVar_ socketStatus $ \_ -> return Bound
--           -- bindSocket sock (SockAddrUnix "x")
--           listen sock maxListenQueue
--           p <- (socketPort sock)
--           print p
--           return sock
--       )
--   where
--     sO_PASSCRED = 16
--     passCredOpt = CustomSockOpt (fromIntegral sOL_SOCKET, sO_PASSCRED)

#else
{- On non-Unix we use a plain socket -}
makeSocket _ = listenOn $ PortNumber aNY_PORT
#endif

connectToPort :: PortID -> IO Handle
connectToPort = connectTo "localhost"

acceptHandle :: Socket -> IO Handle
acceptHandle s = do
  (h, _, _) <- accept s
  return h

portToString :: PortID -> String
portToString = unpack . Base64.encode . encode

stringToPort :: String -> PortID
stringToPort = decode . Base64.decodeLenient . pack


{- Orphans -}
deriving instance Generic PortID
deriving instance Generic PortNumber
instance Binary PortID
instance Binary PortNumber
