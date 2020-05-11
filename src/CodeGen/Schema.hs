{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.Text                      as T
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

  mapM_ (genFile svcName svcVer) schemas

genFile :: ServiceName -> ServiceVersion -> Schema -> IO ()
genFile svcName svcVer desc = do
  name <- get schemaId "schemaId" desc
  let moduleName = T.intercalate "." [svcName, svcVer, schemaName, name]
  print moduleName

  let path    = FP.addExtension (T.unpack name) "hs"
  let content = "module " <> moduleName <> " where"
  B.writeFile path (T.encodeUtf8 content)
