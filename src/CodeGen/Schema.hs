{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT )
import           Discovery.RestDescription
import           Discovery.RestDescription.Schema
import           CodeGen.Types
import           CodeGen.Util                   ( get
                                                , withDir
                                                )

schemaName :: Text
schemaName = "Schema"

schemaDir :: SchemaDir
schemaDir = T.unpack schemaName

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

  (record , jsonObjs) <- runWriterT $ createRecord schema
  (records, refs    ) <- runWriterT $ createFieldRecords jsonObjs

  let path    = FP.addExtension (T.unpack name) "hs"
  let imports = createImports svcName svcVer refs
  let content = T.unlines
        [ "module " <> moduleName <> " where"
        , ""
        , imports
        , ""
        , record
        , ""
        , records
        ]
  B.writeFile path (T.encodeUtf8 content)

createRecord :: Schema -> GenRecord Record
createRecord schema = case schemaType schema of
  (Just (ObjectType obj)) -> do
    name  <- lift $ get schemaId "schema id" schema
    props <- lift $ get objectProperties "object properties" obj
    field <- createField name props
    pure $ createRecordText name field
  _ -> undefined

createField :: RecordName -> ObjectProperties -> GenRecord Field
createField = undefined

createRecordText :: RecordName -> Field -> Record
createRecordText = undefined

createFieldRecords :: [CodeGen.Types.Object] -> GenRef Record
createFieldRecords = undefined

createImports :: ServiceName -> ServiceVersion -> [Ref] -> Imports
createImports = undefined
