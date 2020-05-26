{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema
  ( gen
  )
where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT )
import           Discovery.RestDescription
import           CodeGen.Data                  as Data
import           CodeGen.Types           hiding ( Schema )
import           CodeGen.Util

schemaName :: Text
schemaName = "Schema"

schemaDir :: SchemaDir
schemaDir = T.unpack schemaName

defaultExtentions :: [Text]
defaultExtentions =
  [ "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  ]

defaultImports :: [Text]
defaultImports = ["import qualified Data.Aeson as Aeson"]

gen :: ServiceName -> ServiceVersion -> RestDescriptionSchemas -> IO ()
gen svcName svcVer schemas = withDir schemaDir $ do
  dir <- Dir.getCurrentDirectory
  print dir

  foldM_ (createFile svcName svcVer) Map.empty schemas

createFile
  :: ServiceName -> ServiceVersion -> ImportMap -> Schema -> IO ImportMap
createFile svcName svcVer importMap schema = do
  name <- get schemaId "schemaId" schema

  let moduleName = T.intercalate "." [svcName, svcVer, schemaName, name]
  print moduleName

  createHsBootFile name moduleName schema
  createHsFile svcName svcVer name moduleName importMap schema

createHsBootFile :: RecordName -> ModuleName -> Schema -> IO ()
createHsBootFile name moduleName schema = do
  record <- Data.createBootData schema

  let path = FP.addExtension (T.unpack name) "hs-boot"
      content =
        flip T.snoc '\n'
          . unLines
          $ ["module " <> moduleName <> " where", "import Data.Aeson", record]
  B.writeFile path (T.encodeUtf8 content)

createHsFile
  :: ServiceName
  -> ServiceVersion
  -> RecordName
  -> ModuleName
  -> ImportMap
  -> Schema
  -> IO ImportMap
createHsFile svcName svcVer name moduleName importMap schema = do
  (record , jsonObjs) <- runWriterT $ Data.createData moduleName schema
  (records, imports ) <- runWriterT $ Data.createFieldData moduleName jsonObjs
  let path = FP.addExtension (T.unpack name) "hs"
      importList =
        L.sort
          $  defaultImports
          <> createImports svcName svcVer name importMap imports
      content =
        flip T.snoc '\n'
          . unLines
          $ [ T.intercalate "\n" defaultExtentions
            , "module " <> moduleName <> " where"
            , T.intercalate "\n" importList
            , record
            , records
            ]
  B.writeFile path (T.encodeUtf8 content)

  let names = Set.map unImport . Set.filter filterRecord $ imports
  pure $ if Set.null names
    then importMap
    else do
      let imptMap = Map.singleton name names
      Map.union imptMap importMap
 where
  unImport :: Import -> Text
  unImport (Import imports) = imports
  unImport _                = undefined
  filterRecord :: Import -> Bool
  filterRecord (Import _) = True
  filterRecord _          = False

createImports
  :: ServiceName
  -> ServiceVersion
  -> RecordName
  -> ImportMap
  -> Set Import
  -> [Text]
createImports svcName svcVersion name importMap = fmap f . Set.toList
 where
  f ImportPrelude  = "import RIO"
  f ImportEnum     = "import qualified RIO.Map as Map"
  f ImportGenerics = "import GHC.Generics()"
  f (Import recName) =
    let t = isCyclicImport name recName Set.empty importMap
        s = if t then "{-# SOURCE #-} " else ""
    in  "import "
        <> s
        <> "qualified "
        <> svcName
        <> "."
        <> svcVersion
        <> "."
        <> schemaName
        <> "."
        <> recName
        <> " as "
        <> recName

isCyclicImport
  :: RecordName -> RecordName -> Set RecordName -> ImportMap -> Bool
isCyclicImport name recName acc importMap
  | Set.member recName acc
  = False
  | otherwise
  = maybe
      False
      (\rs -> Set.member name rs || any
        (== True)
        (fmap (\r -> isCyclicImport name r (Set.insert recName acc) importMap)
              (Set.toList rs)
        )
      )
    $ Map.lookup recName importMap
