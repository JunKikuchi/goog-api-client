{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
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

defaultImports :: [Text]
defaultImports = ["import RIO"]

gen :: ServiceName -> ServiceVersion -> RestDescriptionSchemas -> IO ()
gen svcName svcVer schemas = withDir schemaDir $ do
  dir <- Dir.getCurrentDirectory
  print dir

  mapM_ (createFile svcName svcVer) schemas

createFile :: ServiceName -> ServiceVersion -> Schema -> IO ()
createFile svcName svcVer schema = do
  name <- get schemaId "schemaId" schema
  let moduleName = T.intercalate "." [svcName, svcVer, schemaName, name]
  print moduleName

  (record , jsonObjs) <- runWriterT $ Record.createRecord schema
  (records, refs    ) <- runWriterT $ Record.createFieldRecords jsonObjs

  let path    = FP.addExtension (T.unpack name) "hs"
  let imports = createImports svcName svcVer refs
  let content =
        flip T.snoc '\n'
          . T.intercalate "\n\n"
          . filter (not . T.null)
          $ [ "module " <> moduleName <> " where"
            , T.intercalate "\n" defaultImports
            , T.intercalate "\n" imports
            , record
            , records
            ]
  B.writeFile path (T.encodeUtf8 content)

createImports :: ServiceName -> ServiceVersion -> Set Ref -> [Text]
createImports svcName svcVersion = fmap f . Set.toList
 where
  f (Ref ref) =
    "import " <> svcName <> "." <> svcVersion <> "." <> schemaName <> "." <> ref
  f RefGAC = "import qualified GoogApiClient as GAC"
