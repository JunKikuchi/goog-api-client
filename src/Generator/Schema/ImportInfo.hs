{-# LANGUAGE OverloadedStrings #-}
module Generator.Schema.ImportInfo
  ( ImportInfo
  , empty
  , canonicalName
  , insert
  , createImports
  , createImport
  )
where

import           RIO                     hiding ( lookup )
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           Generator.Types
import           Generator.Schema.Types

data ImportInfo
  = ImportInfo
  { importInfoImports :: Map RecordName Imports
  , importInfoRename  :: Map RecordName RecordName
  } deriving (Show)

type Imports = Set RecordName

empty :: ImportInfo
empty =
  ImportInfo {importInfoImports = Map.empty, importInfoRename = Map.empty}

canonicalName :: RecordName -> ImportInfo -> (RecordName, ImportInfo)
canonicalName name importInfo
  | member name importInfo = (cname, rename name cname importInfo)
  | otherwise              = (name, importInfo)
  where cname = name <> "'"

member :: RecordName -> ImportInfo -> Bool
member name importInfo =
  Map.member (T.toUpper name) $ importInfoImports importInfo

lookup :: RecordName -> ImportInfo -> Maybe Imports
lookup name importInfo =
  Map.lookup (T.toUpper name) (importInfoImports importInfo)

rename :: RecordName -> RecordName -> ImportInfo -> ImportInfo
rename name cname importInfo = importInfo
  { importInfoRename = Map.insert name cname $ importInfoRename importInfo
  }

insert :: RecordName -> Set Import -> ImportInfo -> ImportInfo
insert name imports importInfo = importInfo
  { importInfoImports = Map.union newImportInfo $ importInfoImports importInfo
  }
 where
  newImportInfo = Map.singleton (T.toUpper name) names
  names = Set.map (T.toUpper . unImport) . Set.filter filterRecord $ imports
  unImport :: Import -> Text
  unImport (Import impt) = T.toUpper impt
  unImport _             = undefined
  filterRecord :: Import -> Bool
  filterRecord (Import _) = True
  filterRecord _          = False

createImports
  :: ServiceName
  -> ServiceVersion
  -> Text
  -> RecordName
  -> ImportInfo
  -> Set Import
  -> [Text]
createImports svcName svcVersion schemaName name importInfo =
  fmap f . Set.toList
 where
  f ImportPrelude    = "import RIO"
  f ImportText       = "import qualified RIO.Text as T"
  f ImportEnum       = "import qualified RIO.Map as Map"
  f ImportGenerics   = "import GHC.Generics()"
  f (Import recName) = createImport sourcePragma
                                    svcName
                                    svcVersion
                                    schemaName
                                    cname
                                    recName
   where
    sourcePragma = if isCyclic then "{-# SOURCE #-} " else ""
    isCyclic     = isCyclicImport name recName Set.empty importInfo
    cname =
      fromMaybe recName . Map.lookup recName . importInfoRename $ importInfo

createImport
  :: Text
  -> ServiceName
  -> ServiceVersion
  -> Text
  -> RecordName
  -> RecordName
  -> Text
createImport sourcePragma svcName svcVersion schemaName cname recName =
  "import "
    <> sourcePragma
    <> "qualified "
    <> svcName
    <> "."
    <> svcVersion
    <> "."
    <> schemaName
    <> "."
    <> cname
    <> " as "
    <> recName -- cname にしたいところ

isCyclicImport
  :: RecordName -> RecordName -> Set RecordName -> ImportInfo -> Bool
isCyclicImport name recName acc importInfo
  | Set.member rn acc = False
  | otherwise         = maybe False f $ lookup recName importInfo
 where
  n  = T.toUpper name
  rn = T.toUpper recName
  f imports = imported || cyclicImport
   where
    imported = Set.member n imports
    cyclicImport =
      (True `elem`)
        . fmap (\r -> isCyclicImport name r (Set.insert rn acc) importInfo)
        $ Set.toList imports
