{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT )
import           Discovery.RestDescription
import           Discovery.RestDescription.Schema
import           CodeGen.Schema.Record         as Record
import           CodeGen.Types
import           CodeGen.Util

schemaName :: Text
schemaName = "Schema"

schemaDir :: SchemaDir
schemaDir = T.unpack schemaName

gen :: ServiceName -> ServiceVersion -> RestDescriptionSchemas -> IO ()
gen svcName svcVer schemas = withDir schemaDir $ do
  dir <- Dir.getCurrentDirectory
  print dir

  foldM_ (createFile svcName svcVer) Map.empty schemas

type RefRecords = Map RecordName (Set RecordName)

createFile
  :: ServiceName -> ServiceVersion -> RefRecords -> Schema -> IO RefRecords
createFile svcName svcVer refRecs schema = do
  name <- get schemaId "schemaId" schema

  let moduleName = T.intercalate "." [svcName, svcVer, schemaName, name]
  print moduleName

  refs <- createHsFile svcName svcVer name moduleName refRecs schema
  createHsBootFile name moduleName schema
  pure refs

createHsFile
  :: ServiceName
  -> ServiceVersion
  -> RecordName
  -> ModuleName
  -> RefRecords
  -> Schema
  -> IO RefRecords
createHsFile svcName svcVer name moduleName refRecs schema = do
  (record , jsonObjs) <- runWriterT $ Record.createRecord schema
  (records, refs    ) <- runWriterT $ Record.createFieldRecords jsonObjs

  let path    = FP.addExtension (T.unpack name) "hs"
  let imports = createImports svcName svcVer refs
  let content =
        flip T.snoc '\n'
          . unLines
          $ [ "module " <> moduleName <> " where"
            , T.intercalate "\n" imports
            , record
            , records
            ]
  B.writeFile path (T.encodeUtf8 content)

  let names = Set.map unRef . Set.filter filterRecord $ refs
  pure $ if Set.null names
    then refRecs
    else do
      let refRec = Map.singleton name names
      Map.union refRec refRecs
 where
  unRef :: Ref -> Text
  unRef (Ref ref) = ref
  unRef _         = undefined
  filterRecord :: Ref -> Bool
  filterRecord (Ref _) = True
  filterRecord _       = False

createHsBootFile :: RecordName -> ModuleName -> Schema -> IO ()
createHsBootFile name moduleName schema = do
  (record, _) <- runWriterT $ Record.createBootRecord schema

  let path = FP.addExtension (T.unpack name) "hs-boot"
  let content =
        flip T.snoc '\n'
          . unLines
          $ ["module " <> moduleName <> " where", record]
  B.writeFile path (T.encodeUtf8 content)

createImports :: ServiceName -> ServiceVersion -> Set Ref -> [Text]
createImports svcName svcVersion = fmap f . Set.toList
 where
  -- 循環インポート時に {-# SOURCE #-} を追加
  f (Ref ref) =
    "import " <> svcName <> "." <> svcVersion <> "." <> schemaName <> "." <> ref
  f RefGAC     = "import qualified GoogApiClient as GAC"
  f RefPrelude = "import RIO"
