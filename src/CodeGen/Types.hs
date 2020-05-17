module CodeGen.Types where

import           RIO                     hiding ( Enum )
import qualified JSON.Schema                   as JSON
import           RIO.Writer                     ( WriterT )

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

type GenRecord  = WriterT [Gen] IO
type GenRef     = WriterT (Set Ref) IO
data Gen        = Gen Schema | GenEnum Enum | GenRef Ref deriving Show
type Schema     = (SchemaName, JSON.Schema)
type SchemaName = Text
type Enum       = (SchemaName, Enums)
type Enums      = [(EnumName, EnumDesc)]
type EnumName   = Text
type EnumDesc   = Text
data Ref        = Ref Text | RefGAC | RefPrelude deriving (Eq, Ord, Show)
