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
type RecordName     = Text

type GenRecord  = WriterT [Object] IO
type Object     = (ObjectName, JSON.Object)
type ObjectName = Text

type GenRef = WriterT [Ref] IO
type Ref    = Text
