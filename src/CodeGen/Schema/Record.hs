{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema.Record where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT
                                                , tell
                                                )
import qualified Discovery.RestDescription.Schema
                                               as Desc
import qualified JSON.Schema                   as JSON
import           CodeGen.Types
import           CodeGen.Util

createRecord :: ModuleName -> Desc.Schema -> GenRecord Text
createRecord moduleName schema = do
  name <- lift $ get Desc.schemaId "schema id" schema
  let desc = Desc.schemaDescription schema
  schemaType <- get Desc.schemaType "schema type" schema
  case schemaType of
    (Desc.ObjectType obj) -> do
      props    <- createRecordProperties moduleName name desc obj
      addProps <- createRecordAdditionalProperties moduleName name desc obj
      maybe
        (error "faild to get JSON object properties nor additionalProperties")
        pure
        (props <|> addProps)
    (Desc.ArrayType array) -> createArrayRecord moduleName name schema array
    Desc.AnyType           -> createAnyRecord name desc
    _                      -> undefined

createRecordProperties
  :: ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.Object
  -> GenRecord (Maybe Text)
createRecordProperties moduleName name desc obj =
  case Desc.objectProperties obj of
    (Just props) -> createRecordPropertiesContent moduleName name desc props
    _            -> pure Nothing

createRecordPropertiesContent
  :: ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.ObjectProperties
  -> GenRecord (Maybe Text)
createRecordPropertiesContent moduleName name desc props = do
  field <- createField moduleName name props
  let record = createRecordContent name field (Map.size props) desc
      aeson  = createAesonContent moduleName name props
  pure . pure $ record <> "\n\n" <> aeson

createRecordAdditionalProperties
  :: ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.Object
  -> GenRecord (Maybe Text)
createRecordAdditionalProperties moduleName name desc obj =
  case Desc.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema schema)) ->
      createRecordAdditionalPropertiesContent moduleName name desc schema
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> createRecordAdditionalPropertiesContent
      moduleName
      name
      desc
      JSON.Schema
        { JSON.schemaType             = Just JSON.AnyType
        , JSON.schemaTitle            = Nothing
        , JSON.schemaDescription      = Nothing
        , JSON.schemaExamples         = Nothing
        , JSON.schemaComment          = Nothing
        , JSON.schemaEnum             = Nothing
        , JSON.schemaEnumDescriptions = Nothing
        , JSON.schemaConst            = Nothing
        }

createRecordAdditionalPropertiesContent
  :: ModuleName
  -> RecordName
  -> Maybe Desc
  -> JSON.Schema
  -> GenRecord (Maybe Text)
createRecordAdditionalPropertiesContent moduleName name desc schema = do
  fieldType <- createType moduleName name schema True
  let fieldDesc = descContent 4 (JSON.schemaDescription schema)
  let field = "    " <> T.concat ["un", name] <> " :: Map Text " <> fieldType
  pure
    .  pure
    $  createRecordContent name (fieldDesc <> field) 1 desc
    <> " deriving (Aeson.ToJSON, Aeson.FromJSON)"

createArrayRecord
  :: ModuleName -> SchemaName -> Desc.Schema -> Desc.Array -> GenRecord Text
createArrayRecord moduleName name schema array = case Desc.arrayItems array of
  (Just (JSON.ArrayItemsItem fieldSchema)) -> do
    let desc      = Desc.schemaDescription schema
        enumDescs = Desc.schemaEnumDescriptions schema
        arrayName = name <> "Item"
    fieldType <- createType
      moduleName
      arrayName
      (fieldSchema { JSON.schemaDescription      = desc
                   , JSON.schemaEnumDescriptions = enumDescs
                   }
      )
      True
    pure $ "type " <> name <> " = " <> "[" <> fieldType <> "]"
  _ -> undefined

createAnyRecord :: RecordName -> Maybe Desc -> GenRecord Text
createAnyRecord name desc =
  pure
    $  maybe
         ""
         (\s -> "{-|\n" <> (T.unlines . fmap ("  " <>) . T.lines $ s) <> "-}\n")
         desc
    <> "type "
    <> name
    <> " = Aeson.Value"

createBootRecord :: Desc.Schema -> IO Text
createBootRecord schema = do
  schemaType <- get Desc.schemaType "schema type" schema
  name       <- get Desc.schemaId "schema id" schema
  pure $ case schemaType of
    (Desc.ObjectType _) ->
      "data "
        <> name
        <> "\n"
        <> "instance FromJSON "
        <> name
        <> "\n"
        <> "instance ToJSON "
        <> name
        <> "\n"
    (Desc.ArrayType _) ->
      "type " <> name <> " = " <> "[" <> name <> "Item" <> "]"
    Desc.AnyType -> "type " <> name <> " = Aeson.Value"
    _            -> undefined

createField
  :: ModuleName -> RecordName -> Desc.ObjectProperties -> GenRecord Text
createField moduleName name props = do
  fields <- Map.foldrWithKey cons (pure mempty) props
  pure $ T.intercalate ",\n\n" fields
 where
  cons s schema acc = do
    let camelName = name <> toCamelName s
        fieldName = unTitle camelName
        desc      = descContent 4 $ JSON.schemaDescription schema
    fieldType <- createType moduleName camelName schema False
    let field = "    " <> fieldName <> " :: " <> fieldType
    ((desc <> field) :) <$> acc

descContent :: Int -> Maybe Text -> Text
descContent n = maybe
  ""
  (\s ->
    indent
      <> "{-|\n"
      <> (T.unlines . fmap ((indent <> "  ") <>) . T.lines $ s)
      <> indent
      <> "-}\n"
  )
  where indent = T.concat $ take n $ L.repeat " "

createType
  :: ModuleName -> SchemaName -> JSON.Schema -> Required -> GenRecord Text
createType moduleName name schema required = do
  jsonType <- get JSON.schemaType "schemaType" schema
  _type    <- case jsonType of
    (JSON.StringType  _) -> createEnumType "Text" name schema
    (JSON.IntegerType _) -> pure "Int"
    (JSON.NumberType  _) -> pure "Float"
    (JSON.ObjectType _) ->
      tell [Gen (name, schema)] >> pure (moduleName <> "." <> name)
    (JSON.RefType ref) ->
      if Just ref /= L.headMaybe (reverse (T.split (== '.') moduleName)) -- TODO: ここで RecordName が欲しい
        then tell [GenRef (Ref ref)] >> pure (ref <> "." <> ref)
        else pure ref
    (JSON.ArrayType array) -> createArrayType moduleName name schema array
    JSON.BooleanType       -> pure "Bool"
    JSON.AnyType           -> pure "Aeson.Value"
    JSON.NullType          -> undefined
  if required then pure _type else pure $ "Maybe " <> _type

createEnumType :: Text -> SchemaName -> JSON.Schema -> GenRecord Text
createEnumType defaultType name schema = case JSON.schemaEnum schema of
  (Just jsonEnum) -> do
    let descs = fromMaybe mempty $ JSON.schemaEnumDescriptions schema
    tell [GenEnum (name, zip jsonEnum descs), GenRef RefGenerics]
    pure name
  _ -> pure defaultType

createArrayType
  :: ModuleName -> SchemaName -> JSON.Schema -> JSON.Array -> GenRecord Text
createArrayType moduleName name schema array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem fieldSchema)) -> do
    let desc           = JSON.schemaDescription schema
        enumDescs      = JSON.schemaEnumDescriptions schema
        newFieldSchema = if isJust enumDescs
          then fieldSchema { JSON.schemaDescription      = desc
                           , JSON.schemaEnumDescriptions = enumDescs
                           }
          else fieldSchema { JSON.schemaDescription = desc }
    fieldType <- createType moduleName name newFieldSchema True
    pure $ "[" <> fieldType <> "]"
  _ -> undefined

createRecordContent :: RecordName -> Text -> Int -> Maybe Text -> Text
createRecordContent name field size desc
  = maybe
      ""
      (\s -> "{-|\n" <> (T.unlines . fmap ("  " <>) . T.lines $ s) <> "-}\n")
      desc
    <> (if size == 1 then "newtype " else "data ")
    <> name
    <> " = "
    <> name
    <> (if size == 0 then "" else "\n  {\n" <> field <> "\n  }")

createAesonContent :: ModuleName -> RecordName -> Desc.ObjectProperties -> Text
createAesonContent moduleName name props =
  createFromJSONContent moduleName name props
    <> "\n\n"
    <> createToJSONContent moduleName name props

createFromJSONContent
  :: ModuleName -> RecordName -> Desc.ObjectProperties -> Text
createFromJSONContent moduleName name props
  | Map.size props == 0
  = "instance Aeson.FromJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n  parseJSON = Aeson.withObject \""
    <> name
    <> "\" (\\v -> if null v then pure "
    <> moduleName
    <> "."
    <> name
    <> " else mempty)"
  | otherwise
  = "instance Aeson.FromJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n  parseJSON = Aeson.withObject \""
    <> name
    <> "\" $ \\v -> "
    <> moduleName
    <> "."
    <> name
    <> "\n    <$> "
    <> T.intercalate "\n    <*> " (Map.foldrWithKey cons mempty props)
  where cons s _schema acc = ("v Aeson..:?" <> " \"" <> s <> "\"") : acc

createToJSONContent :: ModuleName -> RecordName -> Desc.ObjectProperties -> Text
createToJSONContent moduleName name props
  | Map.size props == 0
  = "instance Aeson.ToJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n"
    <> "  toJSON "
    <> moduleName
    <> "."
    <> name
    <> " = Aeson.object mempty"
  | otherwise
  = "instance Aeson.ToJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n  toJSON(\n    "
    <> moduleName
    <> "."
    <> name
    <> "\n      "
    <> args
    <> "\n    ) = Aeson.object\n    [ "
    <> obj
    <> "\n    ]"
 where
  names =
    (\key -> (key, unTitle name <> toCamelName key <> "'")) <$> Map.keys props
  args = T.intercalate "\n      " (snd <$> names)
  obj  = T.intercalate
    "\n    , "
    ((\(key, argName) -> "\"" <> key <> "\" Aeson..= " <> argName) <$> names)

createFieldRecords :: ModuleName -> [Gen] -> GenRef Text
createFieldRecords moduleName = fmap unLines . foldr f (pure mempty)
 where
  f :: Gen -> GenRef [Text] -> GenRef [Text]
  f (GenRef ref) acc = do
    tell $ Set.singleton ref
    acc
  f (GenEnum (name, enums)) acc = do
    let a     = createFieldEnumContent name enums
        aeson = createFieldEnumAesonContent moduleName name
    ((a <> "\n\n" <> aeson) :) <$> acc
  f (Gen schema) acc = do
    (a, schemas) <- lift $ runWriterT $ createFieldRecord moduleName schema
    if null schemas
      then (a :) <$> acc
      else do
        b <- createFieldRecords moduleName schemas
        (a :) <$> ((b :) <$> acc)

createFieldEnumContent :: SchemaName -> Enums -> Text
createFieldEnumContent name enums =
  "data "
    <> name
    <> "\n  =\n"
    <> T.intercalate
         "\n  |\n"
         (fmap
           (\(e, d) -> descContent 2 (Just d) <> "  " <> name <> toCamelName
             (T.toLower e)
           )
           enums
         )
    <> "\n  deriving (Show, Generic)"

createFieldEnumAesonContent :: ModuleName -> SchemaName -> Text
createFieldEnumAesonContent moduleName name =
  createFieldEnumConstructorTagModifier name
    <> "\n"
    <> createFieldEnumFromJSONContent moduleName name
    <> "\n"
    <> createFieldEnumToJSONContent moduleName name

createFieldEnumConstructorTagModifier :: SchemaName -> Text
createFieldEnumConstructorTagModifier name =
  fn
    <> " :: String -> String\n"
    <> fn
    <> " = drop "
    <> (T.pack . show . T.length $ name)
    <> "\n"
  where fn = "to" <> name

createFieldEnumFromJSONContent :: ModuleName -> SchemaName -> Text
createFieldEnumFromJSONContent moduleName name =
  "instance Aeson.FromJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n"
    <> "  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.constructorTagModifier = to"
    <> name
    <> " }\n"

createFieldEnumToJSONContent :: ModuleName -> SchemaName -> Text
createFieldEnumToJSONContent moduleName name =
  "instance Aeson.ToJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n"
    <> "  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.constructorTagModifier = to"
    <> name
    <> " }\n"

createFieldRecord :: ModuleName -> Schema -> GenRecord Text
createFieldRecord moduleName (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> do
    let desc = JSON.schemaDescription schema
    fields <- createFieldRecordFields moduleName name desc obj
    field  <- createFieldRecordField moduleName name desc obj
    maybe
      (error "faild to get JSON object properties nor additionalProperties")
      pure
      (fields <|> field)
  (Just _) -> undefined
  Nothing  -> undefined

createFieldRecordFields
  :: ModuleName
  -> SchemaName
  -> Maybe Desc
  -> JSON.Object
  -> GenRecord (Maybe Text)
createFieldRecordFields moduleName name desc obj =
  case JSON.objectProperties obj of
    (Just props) -> createRecordPropertiesContent moduleName name desc props
    Nothing      -> pure Nothing

createFieldRecordField
  :: ModuleName
  -> SchemaName
  -> Maybe Desc
  -> JSON.Object
  -> GenRecord (Maybe Text)
createFieldRecordField moduleName name desc obj =
  case JSON.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema schema)) ->
      createRecordAdditionalPropertiesContent moduleName name desc schema
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing
