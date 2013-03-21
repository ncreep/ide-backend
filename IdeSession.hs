{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable #-}
module IdeSession (

  -- | This module provides an interface to the IDE backend.
  --
  -- It centres around the idea of a single threaded IDE session, and
  -- operations for updating the session or running queries given the current
  -- state of the session.

  -- * Interaction with the compiler.
  --
  -- | Ironically for a pure functional language, the interface to the compiler
  -- is rather stateful and sequential. In part this is because it's dealing
  -- with the state of files in the file system which are of course mutable
  -- variables.
  --
  -- So the general pattern of interaction is sequential and single-threaded.
  -- The state transitions are fairly simple:
  --
  -- * update phase: we have a batch of updates, e.g. changes in module contents.
  --   This part is declarative, we just describe what changes we want to make.
  --
  -- * compile phase: we apply the updates and invoke the compiler, which
  --   incrementally recompiles some modules. This may be a relatively
  --   long running operation and we may want progress info.
  --
  -- * query phase: after compiling we can collect information like
  --   source errors, the list of successfully loaded modules
  --   or symbol maps.
  --
  -- * run phase: regardless of compilation results, we may want to run some
  --   code from a module (compiled recently or compiled many updates ago),
  --   interact with the running code's input and output, interrupt
  --   its execution.
  --
  -- Then the whole process can repeat.
  --
  -- To clarify these different phases we use different types:
  --
  -- * 'IdeSession' for the query mode. This is in a sense also the default
  --   mode.
  --
  -- * 'IdeSessionUpdate' for accumulating updates.
  --
  -- * 'Progress' for the progress information in the compile mode.
  --
  -- * 'RunActions' for handles on the running code, through which
  --   one can interact with the code.

  -- * Basic types
  ModuleName,
  LoadedModules,

  -- * Sessions
  IdeSession,

  -- ** Initialisation and shutdown
  -- | Sessions are stateful and must be initialised and shut down so that
  -- resources can be released.
  InProcess,
  initSession,
  shutdownSession,
  SessionConfig(..),
  getSessionConfig,
  getSourcesDir,
  getDataDir,
  restartSession,

  -- * Updates
  -- | Updates are done in batches: we collect together all of the updates we
  -- want to do and then do a single transition, applying all the updates,
  -- and end up in a new state.

  -- ** Declarative updates
  -- | So that we can batch the updates, all the updates are declarative.
  -- The 'IdeSessionUpdate' monoid is used to represent the updates, and the
  -- sub-sections below describe the various updates that are available.
  IdeSessionUpdate,

  -- ** Modules
  updateModule,
  updateModuleFromFile,
  updateModuleDelete,

  -- ** Flags and other settings
  updateGhcOptions,
  updateCodeGeneration,

  -- ** Data files
  updateDataFile,
  updateDataFileFromFile,
  updateDataFileDelete,

  -- ** Environment variables
  updateEnv,

  -- ** Buffer mode
  updateStdoutBufferMode,
  updateStderrBufferMode,

  -- ** Performing the update
  -- | Once we have accumulated a batch of updates we can perform them all
  -- giving us a new session state. Since performing a bunch of updates can
  -- involve compiling modules and can take some time, the update uses the
  -- 'Progress' type to represent intermediate progress information.
  updateSession,
  Progress,
  progressStep,

  -- * Queries
  Query,

  -- ** Source errors
  getSourceErrors,
  SourceError(..),
  SourceErrorKind(..),
  SourceSpan(..),
  EitherSpan(..),

  -- ** Files
  -- | Simply getting the current state of the persistent files fits the
  -- queries pattern.
  getSourceModule,
  getDataFile,

  -- ** The list of managed files and all data files
  ManagedFiles(..),
  getManagedFiles,
  getAllDataFiles,

  -- ** Environment variables
  getEnv,

  -- ** Symbol definition maps
  getLoadedModules,
  IdMap(..),
  IdInfo(..),
  IdNameSpace(..),
  IdScope(..),
  ModuleId (..),
  PackageId (..),
  haddockLink,
  idInfoAtLocation,

  -- ** Import information and autocompletion
  Import(..),
  getImports,
  getAutocompletion,
  idInfoQN,

  -- ** Run code
  runStmt,
  RunResult(..),
  RunBufferMode(..),
  RunActions, -- We don't export the constructor nor all accessors
  interrupt,
  runWait,
  supplyStdin,
  runWaitAll,
  registerTerminationCallback,

  -- ** Start and diagnose the server (probably only for debugging)
  ghcServer,
  getGhcServer,
  getGhcExitCode,

  -- * Additional notes
  -- ** Responsibility for managing and mutating files in the sources dir.
  -- | In general, updating and changing source files in the sources dir has to
  -- be coordinated with the IdeSession, since we're in a concurrent mutable
  -- setting.
  --
  -- The model here is that the IdeSession alone manages the files in the
  -- sources directory. All file changes and file reading must be managed
  -- via the session, and sequenced relative to other session state changes.
  --
  -- The session will manage the files carefully, including in the case of
  -- exceptions and things going awry. Thus the caller does not need to
  -- duplicate the file state: it can rely on putting files in, applying
  -- updates to the files via the session, and extracting the files again
  -- at any time (before the session is closed).

  -- ** Morally pure queries
  -- | Morally, a compiler is a pure function from the current value of the
  -- various source files (and other bits of the environment) to object code
  -- and\/or other information about the modules (errors, types etc).
  --
  -- The intention is to reflect this purity property in this interface. The
  -- value of an 'IdeSession' represents the state of the files\/modules and
  -- contains the other parameters supplied by the user (compiler options,
  -- environment variables). It also contains or represents the result
  -- of the pure compilation function. It should always be the case
  -- that we can throw away all the compilation results and recover
  -- them just from the file state and user parameters.
  --
  -- One example where this notion makes a difference is with warnings.
  -- Traditionally, compilers just return the warnings for the modules they
  -- compiled, skipping warnings for the modules they didn't need to recompile.
  -- But this doesn't match the pure function idea, because the compilation
  -- result now depends on which steps we took to get there, rather than just
  -- on the current value of the files. So one of the things this wrapper can
  -- do is to restore the purity in these corner cases (which otherwise the
  -- client of this API would probably have to do).

  -- ** Persistent and transitory state
  -- | The persistent state is obviously the files: source files and data
  -- files, as well as user-supplied parameters of the compilation.
  -- Internally there is a great deal of transitory and cached state,
  -- either in memory or on disk (such as .hi files on disk or the equivalent
  -- in memory). Note that none of the state persists in case of a fatal
  -- internal error (the files are wiped out before shutdown) and only
  -- the files persist in case of a power failure (but have to be
  -- recovered manually).
  --
  -- It should be possible to drop all the transitory state and recover,
  -- just at the cost of some extra work, as long as the original @Session@
  -- value is available. The 'restartSession' function does almost
  -- exactly that.
  --
  -- This property is a useful correctness property for internal testing: the
  -- results of all the queries should be the same before and after blowing
  -- away all the transitory state and recovering.
) where

import Control.Concurrent (MVar, newMVar)
import qualified Control.Exception as Ex
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import Data.List (delete, isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.Monoid (Monoid (..))
import System.Directory
import System.FilePath (splitFileName, takeDirectory, makeRelative, (<.>), (</>))
import qualified System.FilePath.Find as Find
import System.IO (Handle, hClose, openBinaryTempFile)
import System.IO.Temp (createTempDirectory)
import System.Posix.Files (setFileTimes)
import System.Posix.Types (EpochTime)

import Common
import GhcServer
import GhcRun (RunResult(..), RunBufferMode(..), Import(..))
import GhcHsWalk

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, execStateT)
import Data.Accessor (Accessor, accessor, (.>), (^.), (^=))
import Data.Accessor.Monad.MTL.State (get, modify, set)

import Crypto.Types (BitLength)
import Crypto.Classes (blockLength, initialCtx, updateCtx, finalize)
import Data.Tagged (Tagged, untag)
import Data.Digest.Pure.MD5 (MD5Digest, MD5Context)

import BlockingOps (modifyMVar, modifyMVar_, withMVar)

import Data.Generics

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {
    -- | The directory to use for all session files.
    configDir        :: FilePath
    -- | GHC static options. Can also contain default dynamic options,
    -- that are overridden via session update.
  , configStaticOpts :: [String]
    -- | Should the GHC client run in-process?
  , configInProcess  :: InProcess
  }

data Computed = Computed {
    -- | Last compilation and run errors
    computedErrors :: [SourceError]
    -- | Modules that got loaded okay together with their mappings
    -- from source locations to information about identifiers there
  , computedLoadedModules :: LoadedModules
    -- | Import information. This is (usually) available even for modules
    -- with parsing or type errors
  , computedImports :: Map ModuleName [Import]
    -- | Autocompletion map
    --
    -- Mapping, per module, from prefixes to fully qualified names
    -- I.e., @fo@ might map to @Control.Monad.forM@, @Control.Monad.forM_@
    -- etc. (or possibly to @M.forM@, @M.forM_@, etc when Control.Monad
    -- was imported qualified as @M@).
  , computedAutoMap :: Map ModuleName (Trie [IdInfo])
  }

-- | This type is a handle to a session state. Values of this type
-- point to the non-persistent parts of the session state in memory
-- and to directories containing source and data file that form
-- the persistent part of the session state. Whenever we perform updates
-- or run queries, it's always in the context of a particular handle,
-- representing the session we want to work within. Many sessions
-- can be active at once, but in normal applications this shouldn't be needed.
--
data IdeSession = IdeSession {
    ideStaticInfo :: IdeStaticInfo
  , ideState      :: MVar IdeSessionState
  }

data IdeStaticInfo = IdeStaticInfo {
    -- | Configuration
    ideConfig     :: SessionConfig
    -- | The directory to use for managing source files.
  , ideSourcesDir :: FilePath
    -- | The directory to use for data files that may be accessed by the
    -- running program. The running program will have this as its CWD.
  , ideDataDir    :: FilePath
  }

data IdeSessionState =
    IdeSessionIdle IdeIdleState
  | IdeSessionRunning RunActions IdeIdleState
  | IdeSessionShutdown

type LogicalTimestamp = EpochTime

data IdeIdleState = IdeIdleState {
    -- A workaround for http://hackage.haskell.org/trac/ghc/ticket/7473.
    -- Logical timestamps (used to force ghc to recompile files)
    _ideLogicalTimestamp :: LogicalTimestamp
    -- The result computed by the last 'updateSession' invocation.
  , _ideComputed         :: Maybe Computed
    -- Compiler dynamic options. If they are not set, the options from
    -- SessionConfig are used.
  , _ideNewOpts          :: Maybe [String]
    -- Whether to generate code in addition to type-checking.
  , _ideGenerateCode     :: Bool
    -- Files submitted by the user and not deleted yet.
  , _ideManagedFiles     :: ManagedFilesInternal
    -- Environment overrides
  , _ideEnv              :: [(String, Maybe String)]
    -- The GHC server (this is replaced in 'restartSession')
  , _ideGhcServer        :: GhcServer
    -- Buffer mode for standard output for 'runStmt'
  , _ideStdoutBufferMode :: RunBufferMode
    -- Buffer mode for standard error for 'runStmt'
  , _ideStderrBufferMode :: RunBufferMode
    -- Has the environment (as recorded in this state) diverged from the
    -- environment on the server?
  , _ideUpdatedEnv       :: Bool
    -- Has the code diverged from what has been loaded into GHC on the last
    -- call to 'updateSession'?
  , _ideUpdatedCode      :: Bool
  }

-- | The collection of source and data files submitted by the user.
data ManagedFilesInternal = ManagedFilesInternal
  { _managedSource :: [(FilePath, (MD5Digest, LogicalTimestamp))]
  , _managedData   :: [FilePath]
  }

-- | The collection of source and data files submitted by the user.
data ManagedFiles = ManagedFiles
  { sourceFiles :: [FilePath]
  , dataFiles   :: [FilePath]
  }

-- | Recover the fixed config the session was initialized with.
getSessionConfig :: IdeSession -> SessionConfig
getSessionConfig = ideConfig . ideStaticInfo

-- | Obtain the source files directory for this session.
getSourcesDir :: IdeSession -> FilePath
getSourcesDir = ideSourcesDir . ideStaticInfo

-- | Obtain the data files directory for this session.
getDataDir :: IdeSession -> FilePath
getDataDir = ideDataDir . ideStaticInfo

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession ideConfig'@SessionConfig{configStaticOpts,configInProcess} = do
  configDir <- canonicalizePath $ configDir ideConfig'
  let ideConfig = SessionConfig {..}
  ideSourcesDir <- createTempDirectory configDir "src."
  ideDataDir    <- createTempDirectory configDir "data."
  _ideGhcServer <- forkGhcServer configStaticOpts (Just ideDataDir) configInProcess
  -- The value of _ideLogicalTimestamp field is a workaround for
  -- the problems with 'invalidateModSummaryCache', which itself is
  -- a workaround for http://hackage.haskell.org/trac/ghc/ticket/7478.
  -- We have to make sure that file times never reach 0, because this will
  -- trigger an exception (http://hackage.haskell.org/trac/ghc/ticket/7567).
  -- We rather arbitrary start at Jan 2, 1970.
  ideState <- newMVar $ IdeSessionIdle IdeIdleState {
                          _ideLogicalTimestamp = 86400
                        , _ideComputed         = Nothing
                        , _ideNewOpts          = Nothing
                        , _ideGenerateCode     = False
                        , _ideManagedFiles     = ManagedFilesInternal [] []
                        , _ideEnv              = []
                        , _ideUpdatedEnv       = False
                          -- Make sure 'ideComputed' is set on first call
                          -- to updateSession
                        , _ideUpdatedCode      = True
                        , _ideStdoutBufferMode = RunNoBuffering
                        , _ideStderrBufferMode = RunNoBuffering
                        , _ideGhcServer
                        }
  let ideStaticInfo = IdeStaticInfo{..}
  let session = IdeSession{..}

  return session

-- | Close a session down, releasing the resources.
--
-- This operation is the only one that can be run after a shutdown was already
-- performed. This lets the API user execute an early shutdown, e.g., before
-- the @shutdownSession@ placed inside 'bracket' is triggered by a normal
-- program control flow.
--
-- If code is still running, it will be interrupted.
shutdownSession :: IdeSession -> IO ()
shutdownSession IdeSession{ideState, ideStaticInfo} = do
  snapshot <- $modifyMVar ideState $ \state -> return (IdeSessionShutdown, state)
  case snapshot of
    IdeSessionRunning runActions idleState -> do
      -- We need to terminate the running program before we can shut down
      -- the session, because the RPC layer will sequentialize all concurrent
      -- calls (and if code is still running we still have an active
      -- RPC conversation)
      interrupt runActions
      void $ runWaitAll runActions
      shutdownGhcServer $ _ideGhcServer idleState
      cleanupDirs
    IdeSessionIdle idleState -> do
      shutdownGhcServer $ _ideGhcServer idleState
      cleanupDirs
    IdeSessionShutdown -> return ()
 where
  cleanupDirs = do
    let dataDir    = ideDataDir ideStaticInfo
        sourcesDir = ideSourcesDir ideStaticInfo
    -- TODO: this has a race condition (not sure we care; if not, say why not)
    dataExists <- doesDirectoryExist dataDir
    when dataExists $ removeDirectoryRecursive dataDir
    sourceExists <- doesDirectoryExist sourcesDir
    when sourceExists $ removeDirectoryRecursive sourcesDir

-- | Restarts a session. Technically, a new session is created under the old
-- @IdeSession@ handle, with a state cloned from the old session,
-- which is then shut down. The only behavioural difference between
-- the restarted session and the old one is that any running code is stopped
-- (even if it was stuck and didn't respond to interrupt requests)
-- and that no modules are loaded, though all old modules and data files
-- are still contained in the new session and ready to be compiled with
-- the same flags and environment variables as before.
--
-- (We don't automatically recompile the code using the new session, because
-- what would we do with the progress messages?)
restartSession :: IdeSession -> IO ()
restartSession session@IdeSession{ideStaticInfo, ideState} = do
    $modifyMVar_ ideState $ \state ->
      case state of
        IdeSessionIdle idleState ->
          restart idleState
        IdeSessionRunning runActions idleState -> do
          forceCancel runActions
          restart idleState
        IdeSessionShutdown ->
          fail "Shutdown session cannot be restarted."
  where
    restart :: IdeIdleState -> IO IdeSessionState
    restart idleState = do
      forceShutdownGhcServer $ _ideGhcServer idleState
      server <- forkGhcServer opts workingDir (configInProcess config)
      return . IdeSessionIdle
             . (ideComputed    ^= Nothing)
             . (ideUpdatedEnv  ^= True)
             . (ideUpdatedCode ^= True)
             . (ideGhcServer   ^= server)
             $ idleState

    workingDir = Just (ideDataDir ideStaticInfo)
    config     = ideConfig ideStaticInfo
    opts       = configStaticOpts config

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
newtype IdeSessionUpdate = IdeSessionUpdate {
    runSessionUpdate :: IdeStaticInfo -> StateT IdeIdleState IO ()
  }

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \_ideConfig -> return ()
  (IdeSessionUpdate f) `mappend` (IdeSessionUpdate g) =
    IdeSessionUpdate $ \ideConfig -> f ideConfig >> g ideConfig

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so we support a callback
-- which can be used to monitor progress of the operation.
updateSession :: IdeSession -> IdeSessionUpdate -> (Progress -> IO ()) -> IO ()
updateSession IdeSession{ideStaticInfo, ideState} update callback = do
  $modifyMVar_ ideState $ \state ->
    case state of
      IdeSessionIdle idleState -> do
        idleState' <- execStateT (runSessionUpdate update ideStaticInfo) idleState

        -- Update environment
        when (idleState' ^. ideUpdatedEnv) $
          rpcSetEnv (idleState ^. ideGhcServer) (idleState' ^. ideEnv)

        -- Determine imports and completion map from the diffs sent
        -- from the RPC compilationi process.
        let usePrevious :: IdeIdleState
                        -> Map ModuleName (Maybe ([Import], Trie [IdInfo]))
                        -> ( Map ModuleName [Import]
                           , Map ModuleName (Trie [IdInfo]) )
            usePrevious idleSt importsAuto =
              ( applyMapDiff (Map.map (fmap fst) importsAuto)
                $ maybe Map.empty computedImports $ idleSt ^. ideComputed
              , applyMapDiff (Map.map (fmap snd) importsAuto)
                $ maybe Map.empty computedAutoMap $ idleSt ^. ideComputed
              )
        -- Update code
        computed <- if (idleState' ^. ideUpdatedCode) then do
                      ( computedErrors
                       , computedLoadedModules
                       , importsAuto
                       ) <- rpcCompile (idleState ^. ideGhcServer)
                                       (idleState' ^. ideNewOpts)
                                       (ideSourcesDir ideStaticInfo)
                                       (idleState' ^. ideGenerateCode)
                                       callback
                      let (computedImports, computedAutoMap) =
                            usePrevious idleState' importsAuto
                      return $ Just Computed{..}
                    else return $ idleState' ^. ideComputed

        -- Update state
        return . IdeSessionIdle
               . (ideComputed    ^= computed)
               . (ideUpdatedEnv  ^= False)
               . (ideUpdatedCode ^= False)
               $ idleState'
      IdeSessionRunning _ _ ->
        Ex.throwIO (userError "Cannot update session in running mode")
      IdeSessionShutdown ->
        Ex.throwIO (userError "Session already shut down.")

-- | Writes a file atomically.
--
-- The file is either written successfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
-- Returns the hash of the file; we are careful not to force the entire input
-- bytestring into memory (we compute the hash as we write the file).
writeFileAtomic :: FilePath -> BSL.ByteString -> IO MD5Digest
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  createDirectoryIfMissing True targetDir
  Ex.bracketOnError
    (openBinaryTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        let bits :: Tagged MD5Digest BitLength ; bits = blockLength
        hash <- go handle initialCtx $ makeBlocks (untag bits `div` 8) content
        hClose handle
        renameFile tmpPath targetPath
        return hash)
  where
    go :: Handle -> MD5Context -> [BSS.ByteString] -> IO MD5Digest
    go _ _   []       = error "Bug in makeBlocks"
    go h ctx [bs]     = BSS.hPut h bs >> return (finalize ctx bs)
    go h ctx (bs:bss) = BSS.hPut h bs >> go h (updateCtx ctx bs) bss

-- | @makeBlocks n@ splits a bytestring into blocks with a size that is a
-- multiple of 'n', with one left-over smaller bytestring at the end.
--
-- Based from the (unexported) 'makeBlocks' in the crypto-api package, but
-- we are careful to be as lazy as possible (the first -- block can be returned
-- before the entire input bytestring is forced)
makeBlocks :: Int -> ByteString -> [BSS.ByteString]
makeBlocks n = go . BSL.toChunks
  where
    go [] = [BSS.empty]
    go (bs:bss)
      | BSS.length bs >= n =
          let l = BSS.length bs - (BSS.length bs `rem` n)
              (bsInit, bsTail) = BSS.splitAt l bs
          in bsInit : go (bsTail : bss)
      | otherwise =
          case bss of
            []         -> [bs]
            (bs':bss') -> go (BSS.append bs bs' : bss')

-- | A session update that changes a source module by giving a new value for
-- the module source. This can be used to add a new module or update an
-- existing one. The ModuleName argument determines the directory
-- and file where the module is located within the project. The actual
-- internal compiler module name, such as the one given by the
-- @getLoadedModules@ query, comes from within @module ... end@.
-- Usually the two names are equal, but they needn't be.
--
updateModule :: FilePath -> ByteString -> IdeSessionUpdate
updateModule m bs = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  let internal = internalFile ideSourcesDir m
  old <- get (ideManagedFiles .> managedSource .> lookup' m)
  -- We always overwrite the file, and then later set the timestamp back
  -- to what it was if it turns out the hash was the same. If we compute
  -- the hash first, we would force the entire lazy bytestring into memory
  newHash <- liftIO $ writeFileAtomic internal bs
  case old of
    Just (oldHash, oldTS) | oldHash == newHash ->
      liftIO $ setFileTimes internal oldTS oldTS
    _ -> do
      newTS <- get ideLogicalTimestamp
      liftIO $ setFileTimes internal newTS newTS
      modify ideLogicalTimestamp (+ 1)
      set (ideManagedFiles .> managedSource .> lookup' m) (Just (newHash, newTS))
      set ideUpdatedCode True

-- | Like 'updateModule' except that instead of passing the module source by
-- value, it's given by reference to an existing file, which will be copied.
--
updateModuleFromFile :: FilePath -> IdeSessionUpdate
updateModuleFromFile p = IdeSessionUpdate $ \staticInfo -> do
  -- We just call 'updateModule' because we need to read the file anyway
  -- to compute the hash.
  bs <- liftIO $ BSL.readFile p
  runSessionUpdate (updateModule p bs) staticInfo

-- | A session update that deletes an existing module.
--
updateModuleDelete :: FilePath -> IdeSessionUpdate
updateModuleDelete m = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  liftIO $ removeFile (internalFile ideSourcesDir m)
  set (ideManagedFiles .> managedSource .> lookup' m) Nothing
  set ideUpdatedCode True

-- | Update dynamic compiler flags, including pragmas and packages to use.
-- Warning: only dynamic flags can be set here.
-- Static flags need to be set at server startup.
updateGhcOptions :: (Maybe [String]) -> IdeSessionUpdate
updateGhcOptions opts = IdeSessionUpdate $ \_ -> do
  set ideNewOpts opts
  set ideUpdatedCode True

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt'.
updateCodeGeneration :: Bool -> IdeSessionUpdate
updateCodeGeneration b = IdeSessionUpdate $ \_ -> do
  set ideGenerateCode b
  set ideUpdatedCode True

internalFile :: FilePath -> FilePath -> FilePath
internalFile ideSourcesDir m = ideSourcesDir </> m

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
--
updateDataFile :: FilePath -> ByteString -> IdeSessionUpdate
updateDataFile n bs = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  liftIO $ writeFileAtomic (ideDataDir </> n) bs
  modify (ideManagedFiles .> managedData) (n :)

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file (the second argument),
-- which will be copied.
--
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate
updateDataFileFromFile n p = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  let targetPath = ideDataDir </> n
      targetDir  = takeDirectory targetPath
  liftIO $ createDirectoryIfMissing True targetDir
  liftIO $ copyFile p targetPath
  modify (ideManagedFiles .> managedData) (n :)

-- | A session update that deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate
updateDataFileDelete n = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  liftIO $ removeFile (ideDataDir </> n)
  modify (ideManagedFiles .> managedData) $ delete n

-- | Set an environment variable
--
-- Use @updateEnv var Nothing@ to unset @var@.
updateEnv :: String -> Maybe String -> IdeSessionUpdate
updateEnv var val = IdeSessionUpdate $ \_ -> do
  set (ideEnv .> lookup' var) (Just val)
  set ideUpdatedEnv True

-- | Set buffering mode for snippets' stdout
updateStdoutBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStdoutBufferMode bufferMode = IdeSessionUpdate $ \_ ->
  set ideStdoutBufferMode bufferMode

-- | Set buffering mode for snippets' stderr
updateStderrBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStderrBufferMode bufferMode = IdeSessionUpdate $ \_ ->
  set ideStderrBufferMode bufferMode

-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way; they might update caches, etc.).
--
type Query a = IdeSession -> IO a

-- | Read the current value of one of the source modules.
--
getSourceModule :: FilePath -> Query ByteString
getSourceModule m IdeSession{ideStaticInfo} =
  BSL.readFile $ internalFile (ideSourcesDir ideStaticInfo) m

-- | Read the current value of one of the data files.
--
getDataFile :: FilePath -> Query ByteString
getDataFile n IdeSession{ideStaticInfo} =
  BSL.readFile $ ideDataDir ideStaticInfo </> n

-- | Get any compilation errors or warnings in the current state of the
-- session, meaning errors that GHC reports for the current state of all the
-- source modules.
--
-- Note that in the initial implementation this will only return warnings from
-- the modules that changed in the last update, the intended semantics is that
-- morally it be a pure function of the current state of the files, and so it
-- would return all warnings (as if you did clean and rebuild each time).
--
getSourceErrors :: Query [SourceError]
getSourceErrors IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO [SourceError]
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} ->
        return (mkRelative (ideSourcesDir ideStaticInfo) computedErrors)
      Nothing -> fail "This session state does not admit queries."

-- | Get the collection of files submitted by the user and not deleted yet.
-- The module names are those supplied by the user as the first
-- arguments of the @updateModule@ and @updateModuleFromFile@ calls,
-- as opposed to the compiler internal @module ... end@ module names.
-- Usually the two names are equal, but they needn't be.
getManagedFiles :: Query ManagedFiles
getManagedFiles IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ plugLeaks $ idleState ^. ideManagedFiles
      IdeSessionRunning _ idleState ->
        return $ plugLeaks $ idleState ^. ideManagedFiles
      IdeSessionShutdown ->
        fail "Session already shut down."
 where
  plugLeaks :: ManagedFilesInternal -> ManagedFiles
  plugLeaks files = ManagedFiles { sourceFiles = map fst $ _managedSource files
                                 , dataFiles = _managedData files }

-- | Get the list of correctly compiled modules, as reported by the compiler,
-- together with a mapping from symbol uses to symbol info.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package,
-- what is the type of this symbol and some more information.
-- This information lets us, e.g, construct Haddock URLs for symbols,
-- like @parallel-3.2.0.3/Control-Parallel.html#v:pseq@.
getLoadedModules :: Query LoadedModules
getLoadedModules IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO LoadedModules
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} ->
        return (mkRelative (ideSourcesDir ideStaticInfo) computedLoadedModules)
      Nothing -> fail "This session state does not admit queries."

-- | Get import information
--
-- This information is available even for modules with parse/type errors
getImports :: Query (Map ModuleName [Import])
getImports IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO (Map ModuleName [Import])
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return computedImports
      Nothing -> fail "This session state does not admit queries."

-- | Autocompletion
getAutocompletion :: Query (ModuleName -> String -> [IdInfo])
getAutocompletion IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO (ModuleName -> String -> [IdInfo])
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return (autocomplete computedAutoMap)
      Nothing           -> fail "This session state does not admit queries."

    autocomplete :: Map ModuleName (Trie [IdInfo])
                 -> ModuleName -> String -> [IdInfo]
    autocomplete mapOfTries modName name =
      let name' = BSSC.pack name
          n     = last (BSSC.split '.' name')
      in filter (\idInfo -> name `isInfixOf` idInfoQN idInfo)
           $ concat
           . Trie.elems
           . Trie.submap n
           $ mapOfTries Map.! modName

-- | Is code generation currently enabled?
getCodeGeneration :: Query Bool
getCodeGeneration IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ idleState ^. ideGenerateCode
      IdeSessionRunning _ idleState ->
        return $ idleState ^. ideGenerateCode
      IdeSessionShutdown ->
        fail "Session already shut down."

-- | Get the list of all data files currently available to the session:
-- both the files copied via an update and files created by user code.
getAllDataFiles :: Query [FilePath]
getAllDataFiles IdeSession{ideStaticInfo} =
  Find.find Find.always
            (Find.fileType Find.==? Find.RegularFile)
            (ideDataDir ideStaticInfo)

-- | Get all current environment overrides
getEnv :: Query [(String, Maybe String)]
getEnv IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ idleState ^. ideEnv
      IdeSessionRunning _ idleState ->
        return $ idleState ^. ideEnv
      IdeSessionShutdown ->
        fail "Session already shut down."

-- | Run a given function in a given module (the name of the module
-- is the one between @module ... end@, which may differ from the file name).
-- The function resembles a query, but it's not instantaneous
-- and the running code can be interrupted or interacted with.
runStmt :: IdeSession -> ModuleName -> String -> IO RunActions
runStmt IdeSession{ideState} m fun = do
  $modifyMVar ideState $ \state -> case state of
    IdeSessionIdle idleState ->
     case (idleState ^. ideComputed, idleState ^. ideGenerateCode) of
       (Just comp, True) ->
          -- ideManagedFiles is irrelevant, because only the module name
          -- inside 'module .. where' counts.
          if m `Map.member` computedLoadedModules comp
          then do
            runActions <- rpcRun (idleState ^. ideGhcServer)
                                 m fun
                                 (idleState ^. ideStdoutBufferMode)
                                 (idleState ^. ideStderrBufferMode)
            registerTerminationCallback runActions restoreToIdle
            return (IdeSessionRunning runActions idleState, runActions)
          else fail $ "Module " ++ show m
                      ++ " not successfully loaded, when trying to run code."
       _ ->
        -- This 'fail' invocation is, in part, a workaround for
        -- http://hackage.haskell.org/trac/ghc/ticket/7539
        -- which would otherwise lead to a hard GHC crash,
        -- instead of providing a sensible error message
        -- that we could show to the user.
        fail "Cannot run before the code is generated."
    IdeSessionRunning _ _ ->
      fail "Cannot run code concurrently"
    IdeSessionShutdown ->
      fail "Session already shut down."
  where
    restoreToIdle :: RunResult -> IO ()
    restoreToIdle _ = $modifyMVar_ ideState $ \state -> case state of
      IdeSessionIdle _ ->
        Ex.throwIO (userError "The impossible happened!")
      IdeSessionRunning _ idleState -> do
        return $ IdeSessionIdle idleState
      IdeSessionShutdown ->
        return state

-- | Get the RPC server used by the session.
getGhcServer :: IdeSession -> IO GhcServer
getGhcServer IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $! idleState ^. ideGhcServer
      IdeSessionRunning _ idleState ->
        return $! idleState ^. ideGhcServer
      IdeSessionShutdown ->
        fail "Session already shut down."

-- Make all nested SourceSpans relative to the given directory
mkRelative :: Data a => FilePath -> a -> a
mkRelative path = everywhere (mkT aux)
  where
    aux :: SourceSpan -> SourceSpan
    aux srcSpan = srcSpan {
        spanFilePath = makeRelative path (spanFilePath srcSpan)
      }

{------------------------------------------------------------------------------
  Accessors
------------------------------------------------------------------------------}

ideLogicalTimestamp :: Accessor IdeIdleState LogicalTimestamp
ideComputed         :: Accessor IdeIdleState (Maybe Computed)
ideNewOpts          :: Accessor IdeIdleState (Maybe [String])
ideGenerateCode     :: Accessor IdeIdleState Bool
ideManagedFiles     :: Accessor IdeIdleState ManagedFilesInternal
ideEnv              :: Accessor IdeIdleState [(String, Maybe String)]
ideGhcServer        :: Accessor IdeIdleState GhcServer
ideStdoutBufferMode :: Accessor IdeIdleState RunBufferMode
ideStderrBufferMode :: Accessor IdeIdleState RunBufferMode
ideUpdatedEnv       :: Accessor IdeIdleState Bool
ideUpdatedCode      :: Accessor IdeIdleState Bool

ideLogicalTimestamp = accessor _ideLogicalTimestamp $ \x s -> s { _ideLogicalTimestamp = x }
ideComputed         = accessor _ideComputed         $ \x s -> s { _ideComputed         = x }
ideNewOpts          = accessor _ideNewOpts          $ \x s -> s { _ideNewOpts          = x }
ideGenerateCode     = accessor _ideGenerateCode     $ \x s -> s { _ideGenerateCode     = x }
ideManagedFiles     = accessor _ideManagedFiles     $ \x s -> s { _ideManagedFiles     = x }
ideEnv              = accessor _ideEnv              $ \x s -> s { _ideEnv              = x }
ideGhcServer        = accessor _ideGhcServer        $ \x s -> s { _ideGhcServer        = x }
ideStdoutBufferMode = accessor _ideStdoutBufferMode $ \x s -> s { _ideStdoutBufferMode = x }
ideStderrBufferMode = accessor _ideStderrBufferMode $ \x s -> s { _ideStderrBufferMode = x }
ideUpdatedEnv       = accessor _ideUpdatedEnv       $ \x s -> s { _ideUpdatedEnv       = x }
ideUpdatedCode      = accessor _ideUpdatedCode      $ \x s -> s { _ideUpdatedCode      = x }

managedSource :: Accessor ManagedFilesInternal [(FilePath, (MD5Digest, LogicalTimestamp))]
managedData   :: Accessor ManagedFilesInternal [FilePath]

managedSource = accessor _managedSource $ \x s -> s { _managedSource = x }
managedData   = accessor _managedData   $ \x s -> s { _managedData   = x }
