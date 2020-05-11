module CodeGen.Types where

import           RIO
import qualified JSON.Schema                   as JSON
import           RIO.Writer                     ( WriterT )

type Dist = FilePath

type ProjectName = Text
type ProjectDir  = FilePath

type SrcDir = FilePath

type ServiceDir = FilePath
type ServiceName = Text
type ServiceVersion = Text

type SchemaDir = FilePath

type GenRecord  = WriterT [Object] IO
type Object     = (ObjectName, JSON.Object)
type ObjectName = Text

type GenRef = WriterT [Ref] IO
type Ref    = Text

type RecordName  = Text
type Record      = Text
type Field       = Text
type Imports     = Text
