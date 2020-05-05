{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import           Prelude                        ( print )
import           RIO
import           RIO.Directory                  ( createDirectoryIfMissing )
import           RIO.FilePath                   ( (</>)
                                                , addExtension
                                                )
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as B
import           Discovery.RestDescription

type DestDir     = Text
type ServiceName = Text
type Version     = Text
type SchemaName  = Text

gen :: DestDir -> RestDescription -> IO ()
gen dest desc = do
  serviceName <- T.toTitle <$> getName
  version     <- T.toTitle <$> getVersion
  -- サービスディレクトリ作成
  let serviceDir = T.unpack dest </> T.unpack serviceName </> T.unpack version
  createDirectoryIfMissing True serviceDir
  -- スキーマファイル作成
  schemas <- getSchemas
  createSchemaFiles serviceName version serviceDir schemas
 where
  getName    = get restDescriptionName "failed to get name." desc
  getVersion = get restDescriptionVersion "failed to get version." desc
  getSchemas = get restDescriptionSchemas "failed to get schemas." desc

createSchemaFiles
  :: ServiceName -> Version -> FilePath -> RestDescriptionSchemas -> IO ()
createSchemaFiles serviceName version serviceDir schemas =
  forM_ schemas $ \schema -> do
    -- スキーマディレクトリ作成
    let dir = serviceDir </> "Schemas"
    createDirectoryIfMissing True dir
    -- スキーマファイル出力
    schemaName <- get restDescriptionParameterId
                      "failed to get schema id."
                      schema
    let path = addExtension (dir </> T.unpack schemaName) "hs"
    print path
    B.writeFile
      path
      (T.encodeUtf8 (createSchema serviceName version schemaName schema))

createSchema
  :: ServiceName -> Version -> SchemaName -> RestDescriptionParameter -> Text
createSchema serviceName version name _ = T.unlines [moduleDef]
 where
  moduleDef  = T.intercalate " " ["module", moduleName, "where"]
  moduleName = T.intercalate "." [serviceName, version, "Schemas", name]

get :: Applicative f => (t -> Maybe a) -> String -> t -> f a
get f s desc = maybe (error s) pure (f desc)
