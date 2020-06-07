module CodeGen.ImportInfo where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           CodeGen.Types

empty :: ImportInfo
empty =
  ImportInfo {importInfoImports = Map.empty, importInfoRename = Map.empty}

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
