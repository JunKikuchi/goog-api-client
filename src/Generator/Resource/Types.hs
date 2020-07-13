module Generator.Resource.Types where

import           RIO                     hiding ( Data
                                                , Enum
                                                )
import           Generator.Types

type GenData = Gen [Data]

data Data = DataEnum Enum | DataImport Import deriving Show

type Enum = (Text, Maybe Text, EnumList)

type EnumList = [(EnumName, EnumDesc)]

type EnumName = Text
type EnumDesc = Text
