{-# LANGUAGE TemplateHaskell #-}
-- | The private types
module IdeSession.Types.Private (
    -- * Types without a public counterpart
    FilePathPtr(..)
  , IdPropPtr(..)
    -- * Types with a public counterpart
  , Public.IdNameSpace(..)
  , IdInfo(..)
  , IdProp(..)
  , IdScope(..)
  , SourceSpan(..)
  , EitherSpan(..)
  , SourceError(..)
  , Public.SourceErrorKind(..)
  , Public.ModuleName
  , ModuleId(..)
  , PackageId(..)
  , IdMap(..)
  , LoadedModules
  , Public.Import(..)
    -- * Cache
  , ExplicitSharingCache(..)
    -- * Util
  , unlocatedSourceError
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import Data.Aeson.TH (deriveJSON)

import qualified IdeSession.Types.Public as Public
import IdeSession.Strict.Container

newtype FilePathPtr = FilePathPtr { filePathPtr :: Int }
  deriving (Eq, Ord)

newtype IdPropPtr = IdPropPtr { idPropPtr :: Int }
  deriving (Eq, Ord)

data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdPropPtr
  , idScope :: !IdScope
  }

data IdProp = IdProp {
    idName  :: !Text
  , idSpace :: !Public.IdNameSpace
  , idType  :: !(Strict Maybe Text)
  }
  deriving (Eq)

data IdScope =
    -- | This is a binding occurrence (@f x = ..@, @\x -> ..@, etc.)
    Binder
    -- | Defined within this module
  | Local {
        idDefSpan :: !EitherSpan
      }
    -- | Imported from a different module
  | Imported {
        idDefSpan      :: !EitherSpan
      , idDefinedIn    :: {-# UNPACK #-} !ModuleId
      , idImportedFrom :: {-# UNPACK #-} !ModuleId
      , idImportSpan   :: !EitherSpan
        -- | Qualifier used for the import
        --
        -- > IMPORTED AS                       idImportQual
        -- > import Data.List                  ""
        -- > import qualified Data.List        "Data.List."
        -- > import qualified Data.List as L   "L."
      , idImportQual   :: !Text
      }
    -- | Wired into the compiler (@()@, @True@, etc.)
  | WiredIn
  deriving (Eq)

data SourceSpan = SourceSpan
  { spanFilePath   :: {-# UNPACK #-} !FilePathPtr
  , spanFromLine   :: {-# UNPACK #-} !Int
  , spanFromColumn :: {-# UNPACK #-} !Int
  , spanToLine     :: {-# UNPACK #-} !Int
  , spanToColumn   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord)

data EitherSpan =
    ProperSpan {-# UNPACK #-} !SourceSpan
  | TextSpan !Text
  deriving (Eq)

data SourceError = SourceError
  { errorKind :: !Public.SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !Text
  }
  deriving (Eq)

data ModuleId = ModuleId
  { moduleName    :: !Public.ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving (Eq)

data PackageId = PackageId
  { packageName    :: !Text
  , packageVersion :: !(Strict Maybe Text)
  }
  deriving (Eq)

newtype IdMap = IdMap { idMapToMap :: Strict (Map SourceSpan) IdInfo }

type LoadedModules = Strict (Map Public.ModuleName) IdMap

{------------------------------------------------------------------------------
  Cache
------------------------------------------------------------------------------}

-- TODO: Since the ExplicitSharingCache contains internal types, resolving
-- references to the cache means we lose implicit sharing because we need
-- to translate on every lookup. To avoid this, we'd have to introduce two
-- versions of the cache and translate the entire cache first.
data ExplicitSharingCache = ExplicitSharingCache {
    filePathCache :: !(Strict IntMap ByteString)
  , idPropCache   :: !(Strict IntMap IdProp)
  }

{------------------------------------------------------------------------------
  JSON
------------------------------------------------------------------------------}

$(deriveJSON id ''FilePathPtr)
$(deriveJSON id ''SourceSpan)
$(deriveJSON id ''EitherSpan)
$(deriveJSON id ''SourceError)
$(deriveJSON id ''IdInfo)
$(deriveJSON id ''IdScope)
$(deriveJSON id ''IdPropPtr)
$(deriveJSON id ''ModuleId)
$(deriveJSON id ''PackageId)
$(deriveJSON id ''IdProp)
$(deriveJSON id ''IdMap)
$(deriveJSON id ''ExplicitSharingCache)

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

unlocatedSourceError :: String -> SourceError
unlocatedSourceError str =
  SourceError Public.KindError
              (TextSpan (Text.pack "No location info"))
              (Text.pack str)
