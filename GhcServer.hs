{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This is the place where the GHC-specific part joins the part
-- implementing the general RPC infrastructure.
--
-- The modules importing any GHC internals, as well as the modules
-- implementing the  RPC infrastructure, should be accessible to the rest
-- of the program only indirectly, through the @GhcServer@ module.
module GhcServer
  ( -- * Types involved in the communication
    PCounter, GhcRequest(..), GhcResponse(..)
    -- * A handle to the server
  , GhcServer
    -- * Client-side operations
  , forkGhcServer
  , rpcGhcServer
  , shutdownGhcServer
  ) where

-- getExecutablePath is in base only for >= 4.6
import qualified Control.Exception as Ex
import Data.Aeson.TH (deriveJSON)
import Data.IORef
import Control.Applicative

import RpcServer
import Common
import GhcRun
import Progress

data GhcRequest
  = ReqCompile (Maybe [String]) FilePath Bool
  | ReqRun     (String, String)
  deriving Show
data GhcResponse = RespWorking PCounter | RespDone RunOutcome
  deriving Show

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcResponse)

-- Keeps the dynamic portion of the options specified at server startup
-- (they are among the options listed in SessionConfig).
-- They are only fed to GHC if no options are set via a session update command.
data GhcInitData = GhcInitData { dOpts :: DynamicOpts
                               , errsRef :: IORef [SourceError]
                               }

type GhcServer = RpcServer GhcRequest GhcResponse

-- * Server-side operations

-- TODO: Do we want to return partial error information while it's
-- generated by runGHC, e.g., warnings? We could either try to run checkModule
-- file by file (do depanalSource and then DFS over the resulting graph,
-- doing \ m -> load (LoadUpTo m)) or rewrite collectSrcError to place
-- warnings in an mvar instead of IORef and read from it into Progress,
-- as soon as they appear.
-- | This function runs in end endless loop, most of which takes place
-- inside the @Ghc@ monad, making incremental compilation possible.
ghcServerEngine :: [String]
                -> RpcServerActions GhcRequest GhcResponse GhcResponse
                -> IO ()
ghcServerEngine opts RpcServerActions{..} = do
  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts opts
  -- Init error collection and define the exception handler.
  errsRef <- newIORef []
  let handleOtherErrors =
        Ex.handle $ \e -> do
          debug dVerbosity $ "handleOtherErrors: " ++ showExWithClass e
          let exError = OtherError (show (e :: Ex.SomeException))
          -- In case of an exception, don't lose saved errors.
          errs <- reverse <$> readIORef errsRef
          -- Don't disrupt the communication.
          putResponse $ RespDone (errs ++ [exError], Nothing)
          -- Restart the Ghc session.
          startGhcSession
      startGhcSession =
        handleOtherErrors $ runFromGhc $ dispatcher GhcInitData{..}

  startGhcSession

 where
  dispatcher :: GhcInitData -> Ghc ()
  dispatcher ghcInitData = do
    req <- liftToGhc $ getRequest
    resp <- ghcServerHandler ghcInitData putProgress req
    liftToGhc $ putResponse resp
    dispatcher ghcInitData

ghcServerHandler :: GhcInitData -> (GhcResponse -> IO ()) -> GhcRequest
                 -> Ghc GhcResponse
ghcServerHandler GhcInitData{dOpts, errsRef}
                 reportProgress
                 (ReqCompile ideNewOpts configSourcesDir ideGenerateCode) = do
  -- Setup progress counter. It goes from [1/n] onwards.
  counterIORef <- liftToGhc $ newIORef 1
  let dynOpts = maybe dOpts optsToDynFlags ideNewOpts
      -- Let GHC API print "compiling M ... done." for each module.
      verbosity = 1
      -- TODO: verify that _ is the "compiling M" message
      handlerOutput msg = do
        oldCounter <- readIORef counterIORef
        putStrLn $ "~~~~~~ " ++ show oldCounter ++ ": " ++ msg
        modifyIORef counterIORef (+1)
        reportProgress (RespWorking oldCounter)
      handlerRemaining _ = return ()  -- TODO: put into logs somewhere?
  errs <- compileInGhc configSourcesDir dynOpts
                       ideGenerateCode verbosity
                       errsRef handlerOutput handlerRemaining
  liftToGhc $ debug dVerbosity "returned from compileInGhc"
  return (RespDone (errs, Nothing))
ghcServerHandler GhcInitData{errsRef} _ (ReqRun funToRun) = do
  runOutcome <- runInGhc funToRun errsRef
  liftToGhc $ debug dVerbosity "returned from runInGhc"
  return (RespDone runOutcome)

-- * Client-side operations

forkGhcServer :: [String] -> IO GhcServer
forkGhcServer opts =
  forkRpcServer (ghcServerEngine opts)

rpcGhcServer :: GhcServer -> GhcRequest
             -> (Progress GhcResponse GhcResponse -> IO a) -> IO a
rpcGhcServer = rpcWithProgress

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer gs = shutdown gs
