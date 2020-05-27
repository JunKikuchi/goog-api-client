module CodeGen.Types where

import           RIO                     hiding ( Enum )
import           RIO.Writer                     ( WriterT )
import qualified JSON.Schema                   as JSON

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

type GenData    = WriterT [Gen] IO
type GenImport  = WriterT (Set Import) IO
data Gen        = GenSchema Schema | GenEnum Enum | GenImport Import deriving Show
type Schema     = (SchemaName, JSON.Schema)
type SchemaName = Text
type Enum       = (SchemaName, EnumList)
type EnumList   = [(EnumName, EnumDesc)]
type EnumName   = Text
type EnumDesc   = Text
data Import     = ImportPrelude | ImportEnum | ImportGenerics | Import RecordName deriving (Eq, Ord, Show)

data ImportInfo
  = ImportInfo
  { importInfoImports :: Map RecordName Imports
  , importInfoRename  :: Map RecordName RecordName
  }
type Imports = Set RecordName
