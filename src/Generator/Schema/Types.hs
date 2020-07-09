module Generator.Schema.Types where

import           RIO                     hiding ( Data
                                                , Enum
                                                )
import qualified JSON
import           Generator.Types

type SchemaDir = FilePath

type ModuleName   = Text
type RecordName   = Text
type ResourceName = Text

type GenData = Gen [Data]

data Data       = DataSchema Schema | DataEnum Enum | DataImport Import deriving Show

type Schema     = (SchemaName, JSON.Schema)
type SchemaName = Text

type Enum       = (SchemaName, EnumList)
type EnumList   = [(EnumName, EnumDesc)]
type EnumName   = Text
type EnumDesc   = Text
