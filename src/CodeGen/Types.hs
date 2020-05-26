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

type GenRecord  = WriterT [Gen] IO
type GenRef     = WriterT (Set Ref) IO
data Gen        = GenSchema Schema | GenEnum Enum | GenRef Ref deriving Show
type Schema     = (SchemaName, JSON.Schema)
type SchemaName = Text
type Enum       = (SchemaName, Enums)
type Enums      = [(EnumName, EnumDesc)]
type EnumName   = Text
type EnumDesc   = Text
data Ref        = RefPrelude | Ref RecordName | RefEnum | RefGenerics deriving (Eq, Ord, Show)
