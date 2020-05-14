module CodeGen.Types where

import           RIO
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
data Gen        = GenObject Object | GenRef Ref deriving Show
type Object     = (ObjectName, JSON.Object)
type ObjectName = Text
data Ref        = Ref Text | RefGAC | RefPrelude deriving (Eq, Ord, Show)
