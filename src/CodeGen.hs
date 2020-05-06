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

defaultImpots :: [Text]
defaultImpots = ["RIO"]

createSchemaFiles
  :: ServiceName -> Version -> FilePath -> RestDescriptionSchemas -> IO ()
createSchemaFiles serviceName version serviceDir schemas = forM_ schemas
  $ \schema -> createSchemaFile serviceName version serviceDir schema

createSchemaFile :: ServiceName -> Version -> FilePath -> Schema -> IO ()
createSchemaFile serviceName version serviceDir schema = do
  -- スキーマディレクトリ作成
  let dir = serviceDir </> "Schemas"
  createDirectoryIfMissing True dir
  -- スキーマファイルパス
  schemaName <- get schemaId "schema id" schema
  let path = addExtension (dir </> T.unpack schemaName) "hs"
  print path
  -- スキーマファイル
  schemaText <- createSchemaText serviceName version schema
  -- スキーマファイル出力
  B.writeFile path (T.encodeUtf8 schemaText)

createSchemaText :: ServiceName -> Version -> Schema -> IO Text
createSchemaText serviceName version schema = do
  moduleDef           <- createModuleDef serviceName version schema
  importsDef          <- createImportsDef
  (dataDef, jsonObjs) <- createDataDef schema
  jsonObjDefs         <- createJsonObjDefs jsonObjs
  pure
    . flip T.snoc '\n'
    . T.intercalate "\n\n"
    . filter (not . T.null)
    $ [moduleDef, importsDef, dataDef, jsonObjDefs]

createModuleDef :: ServiceName -> Version -> Schema -> IO Text
createModuleDef serviceName version schema = do
  schemaName <- get schemaId "schema id" schema
  let moduleDef =
        T.intercalate "." [serviceName, version, "Schemas", schemaName]
  pure $ T.intercalate " " ["module", moduleDef, "where"]

createImportsDef :: IO Text
createImportsDef =
  pure
    $ T.intercalate "\n"
    . fmap (\s -> T.intercalate " " ["import", s])
    $ defaultImpots

createDataDef :: Schema -> IO (Text, [(ObjectName, JSON.Object)])
createDataDef schema = case schemaType schema of
  (Just (ObjectType object)) -> do
    schemaName               <- get schemaId "schema id" schema
    properties               <- get objectProperties "object properties" object
    (recordFields, jsonObjs) <- createRecordFieldsDef schemaName properties
    let dataDef = T.intercalate
          " "
          [ "data"
          , schemaName
          , "="
          , schemaName
          , "\n  {"
          , recordFields
          , "\n  }"
          , "deriving"
          , "Show"
          ]
    pure (dataDef, jsonObjs)
  _ -> pure ("{-- TODO: 未実装 (createDataDef:schemaType) --}", [])

createRecordFieldsDef
  :: SchemaName -> ObjectProperties -> IO (Text, [(ObjectName, JSON.Object)])
createRecordFieldsDef schemaName properties = do
  (fields, jsonObjs) <- Map.foldrWithKey consFiled (pure ([], [])) properties
  let filedsDef = T.intercalate "\n  , " fields
  pure (filedsDef, catMaybes jsonObjs)
 where
  consFiled name schema acc = do
    (fields, jsonObjs) <- acc
    (field , jsonObj ) <- createRecordFieldDef schemaName name schema
    pure (field : fields, jsonObj : jsonObjs)

createRecordFieldDef
  :: SchemaName
  -> Text
  -> JSON.Schema
  -> IO (Text, Maybe (ObjectName, JSON.Object))
createRecordFieldDef schemaName name schema = do
  (filedType, jsonObj) <- createFieldTypeDef objName schema
  pure (T.intercalate " " [fieldName, "::", filedType], jsonObj)
 where
  objName   = toTitle fieldName
  fieldName = T.concat [unTitle schemaName, toTitle camelName]
  camelName = T.concat . fmap toTitle . T.split (== '_') $ name

createFieldTypeDef
  :: ObjectName -> JSON.Schema -> IO (Text, Maybe (ObjectName, JSON.Object))
createFieldTypeDef objName schema = case JSON.schemaType schema of
  (Just (JSON.StringType  _    )) -> pure ("Text", Nothing)
  (Just (JSON.IntegerType _    )) -> pure ("Int", Nothing)
  (Just (JSON.NumberType  _    )) -> pure ("Float", Nothing)
  (Just (JSON.ObjectType  obj  )) -> pure (objName, Just (objName, obj))
  (Just (JSON.ArrayType   array)) -> createArrayFiledDef objName array
  (Just JSON.BooleanType        ) -> pure ("Bool", Nothing)
  (Just (JSON.RefType ref)      ) -> pure (ref, Nothing)
  _ -> pure ("{-- TODO: 未実装 (createFieldTypeDef:schemaType) --}", Nothing)

createArrayFiledDef
  :: ObjectName -> JSON.Array -> IO (Text, Maybe (ObjectName, JSON.Object))
createArrayFiledDef objName array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem item)) -> do
    (fieldType, jsonObj) <- createFieldTypeDef objName item
    pure (T.concat ["[", fieldType, "]"], jsonObj)
  _ -> pure ("{-- TODO: 未実装 (createFieldTypeDef:arrayItems) --}", Nothing)

createJsonObjDefs :: [(ObjectName, JSON.Object)] -> IO Text
createJsonObjDefs = fmap (T.intercalate "\n\n") . foldr
  (\a acc -> do
    dataDef <- createJsonDataDef a
    as      <- acc
    pure (dataDef : as)
  )
  (pure [])

createJsonDataDef :: (ObjectName, JSON.Object) -> IO Text
createJsonDataDef (objName, jsonObj) = do
  recordFields <- createJsonRecordFieldsDef objName jsonObj
  recordField  <- createJsonRecordFieldDef objName jsonObj
  maybe (error "faild to get JSON object properties nor additionalProperties")
        pure
        (recordFields <|> recordField)

createJsonRecordFieldsDef :: ObjectName -> JSON.Object -> IO (Maybe Text)
createJsonRecordFieldsDef objName jsonObj =
  case JSON.objectProperties jsonObj of
    (Just properties) -> do
      (field, _) <- createRecordFieldsDef objName properties
      let dataDef = T.intercalate
            " "
            [ if Map.size properties > 1 then "data" else "newtype"
            , objName
            , "="
            , objName
            , "\n  {"
            , field
            , "\n  }"
            , "deriving"
            , "Show"
            ]
      pure (Just dataDef)
    Nothing -> pure Nothing

createJsonRecordFieldDef :: ObjectName -> JSON.Object -> IO (Maybe Text)
createJsonRecordFieldDef objName jsonObj =
  case JSON.objectAdditionalProperties jsonObj of
    (Just (JSON.AdditionalPropertiesSchema schema)) -> do
      (typeDef, _) <- createFieldTypeDef objName schema
      let dataDef = T.intercalate
            " "
            [ "newtype"
            , objName
            , "="
            , objName
            , "\n  {"
            , T.concat ["un", objName]
            , "::"
            , "Map"
            , "Text"
            , typeDef
            , "\n  }"
            , "deriving"
            , "Show"
            ]
      pure (Just dataDef)
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing

toTitle :: Text -> Text
toTitle = applyHead C.toUpper

unTitle :: Text -> Text
unTitle = applyHead C.toLower

applyHead :: (Char -> Char) -> Text -> Text
applyHead f text = maybe text (\(c, t) -> T.cons (f c) t) (T.uncons text)

get :: Applicative f => (t -> Maybe a) -> Text -> t -> f a
get f s desc = maybe (error err) pure (f desc)
  where err = T.unpack $ T.intercalate " " ["failed to get", s]
