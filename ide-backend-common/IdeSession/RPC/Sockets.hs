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
import Network.Socket hiding (close, accept, socketPort)
import qualified Network.Socket as Socket

newtype ReadChannel = ReadChannel PortID deriving (Generic, Typeable)
newtype WriteChannel = WriteChannel PortID deriving (Generic, Typeable)

instance Binary ReadChannel
instance Binary WriteChannel

makeSocket :: IO Socket
makeSocket = listenOn $ PortNumber aNY_PORT

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
