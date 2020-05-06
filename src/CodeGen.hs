{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import           Prelude                        ( print )
import           RIO                     hiding ( Integer
                                                , String
                                                )
import           RIO.Directory                  ( createDirectoryIfMissing )
import           RIO.FilePath                   ( (</>)
                                                , addExtension
                                                )
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as B
import           Discovery.RestDescription
import           Discovery.RestDescription.Schema

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
  getName    = get restDescriptionName "name" desc
  getVersion = get restDescriptionVersion "version" desc
  getSchemas = get restDescriptionSchemas "schemas" desc

createSchemaFiles
  :: ServiceName -> Version -> FilePath -> RestDescriptionSchemas -> IO ()
createSchemaFiles serviceName version serviceDir schemas =
  forM_ schemas $ \schema -> do
    -- スキーマディレクトリ作成
    let dir = serviceDir </> "Schemas"
    createDirectoryIfMissing True dir
    -- スキーマファイルパス
    schemaName <- get schemaId "schema id" schema
    let path = addExtension (dir </> T.unpack schemaName) "hs"
    print path
    -- スキーマファイル
    schemaText <- createSchema serviceName version schema
    -- スキーマファイル出力
    B.writeFile path (T.encodeUtf8 schemaText)

createSchema :: ServiceName -> Version -> Schema -> IO Text
createSchema serviceName version schema = do
  moduleDef  <- createModuleDef serviceName version schema
  importsDef <- createImportsDef
  typeDef    <- createTypeDef schema
  pure (T.unlines [moduleDef, importsDef, typeDef])

createModuleDef :: ServiceName -> Version -> Schema -> IO Text
createModuleDef serviceName version schema = do
  schemaName <- get schemaId "schema id" schema
  let moduleDef =
        T.intercalate "" [serviceName, version, "Schemas", schemaName]
  pure $ T.intercalate " " ["module", moduleDef, "where"]

createImportsDef :: IO Text
createImportsDef =
  pure $ T.unlines . fmap (\s -> T.intercalate " " ["import", s]) $ ["RIO"]

createTypeDef :: Schema -> IO Text
createTypeDef schema = case schemaType schema of
  (Just (ObjectType _)) -> do
    schemaName <- get schemaId "schema id" schema
    pure $ T.intercalate
      " "
      ["data", schemaName, "=", schemaName, "{", "}", "deriving", "Show"]
  _ -> pure "-- 未対応"

get :: Applicative f => (t -> Maybe a) -> Text -> t -> f a
get f s desc = maybe (error err) pure (f desc)
  where err = T.unpack $ T.intercalate " " ["failed to get", s, "."]
