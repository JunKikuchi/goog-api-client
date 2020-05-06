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
import qualified RIO.Char                      as C
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as B
import qualified RIO.Map                       as Map
import qualified JSON.Schema                   as JSON
import           Discovery.RestDescription
import           Discovery.RestDescription.Schema

type DestDir     = Text
type ServiceName = Text
type Version     = Text
type SchemaName  = Text
type ObjectName  = Text

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
  (Just (ObjectType object)) -> do
    schemaName   <- get schemaId "schema id" schema
    properties   <- get objectProperties "object properties" object
    recordFields <- createRecordFieldsDef schemaName properties
    pure $ T.intercalate
      " "
      [ "data"
      , schemaName
      , "="
      , schemaName
      , "\n{"
      , recordFields
      , "\n}"
      , "deriving"
      , "Show"
      ]
  _ -> pure "{-- TODO: 未実装 (createTypeDef:schemaType) --}"

createRecordFieldsDef :: SchemaName -> ObjectProperties -> IO Text
createRecordFieldsDef schemaName =
  fmap (T.intercalate "\n ,")
    . foldr (\field acc -> (:) <$> field <*> acc) (pure [])
    . Map.foldrWithKey
        (\name schema acc -> createFieldDef schemaName name schema : acc)
        []

createFieldDef :: SchemaName -> Text -> JSON.Schema -> IO Text
createFieldDef schemaName name schema = do
  filedType <- createFieldTypeDef objName schema
  pure $ T.intercalate " " [fieldName, "::", filedType]
 where
  objName   = toTitle fieldName
  fieldName = T.concat [unTitle schemaName, toTitle name]

createFieldTypeDef :: ObjectName -> JSON.Schema -> IO Text
createFieldTypeDef objName schema = case JSON.schemaType schema of
  (Just (JSON.StringType  _    )) -> pure "Text"
  (Just (JSON.IntegerType _    )) -> pure "Int"
  (Just (JSON.NumberType  _    )) -> pure "Float"
  (Just (JSON.ObjectType  _    )) -> pure objName
  (Just (JSON.ArrayType   array)) -> createArrayFiledDef objName array
  (Just JSON.BooleanType        ) -> pure "Bool"
  (Just (JSON.RefType ref)      ) -> pure ref
  _ -> pure "{-- TODO: 未実装 (createFieldTypeDef:schemaType) --}"

createArrayFiledDef :: ObjectName -> JSON.Array -> IO Text
createArrayFiledDef objName array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem item)) -> do
    fieldType <- createFieldTypeDef objName item
    pure $ T.concat ["[", fieldType, "]"]
  _ -> pure "{-- TODO: 未実装 (createFieldTypeDef:arrayItems) --}"

toTitle :: Text -> Text
toTitle = applyHead C.toUpper

unTitle :: Text -> Text
unTitle = applyHead C.toLower

applyHead :: (Char -> Char) -> Text -> Text
applyHead f text = maybe text (\(c, t) -> T.cons (f c) t) (T.uncons text)

get :: Applicative f => (t -> Maybe a) -> Text -> t -> f a
get f s desc = maybe (error err) pure (f desc)
  where err = T.unpack $ T.intercalate " " ["failed to get", s, "."]
