module CodeGen.Types where

import           RIO                     hiding ( Data
                                                , Enum
                                                )
import           RIO.Writer                     ( WriterT )
import qualified JSON.Schema                   as JSON

newtype CodeGenException = GetException Text deriving Show
instance Exception CodeGenException

type Dist       = FilePath
type ProjectDir = FilePath
type SrcDir     = FilePath
type ServiceDir = FilePath
type SchemaDir  = FilePath

type ProjectName    = Text
type ServiceName    = Text
type ServiceVersion = Text
type ModuleName     = Text
type RecordName     = Text
type Desc           = Text

type Required = Bool

type Gen w     = WriterT w
type GenData   = Gen [Data]
type GenImport = Gen (Set Import)

data Data       = DataSchema Schema | DataEnum Enum | DataImport Import deriving Show
type Schema     = (SchemaName, JSON.Schema)
type SchemaName = Text
type Enum       = (SchemaName, EnumList)
type EnumList   = [(EnumName, EnumDesc)]
type EnumName   = Text
type EnumDesc   = Text
data Import     = ImportPrelude | ImportEnum | ImportGenerics | Import RecordName deriving (Eq, Ord, Show)

data ImportInfo
  = ImportInfo
  { importInfoNames   :: Imports
  , importInfoImports :: Map RecordName Imports
  , importInfoRename  :: Map RecordName RecordName
  } deriving (Show)
type Imports = Set RecordName
