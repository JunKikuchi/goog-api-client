{-# LANGUAGE OverloadedStrings #-}
module Generator.Schema.Data
  ( createData
  , createBootData
  , createFieldData
  )
where

import           RIO                     hiding ( Data )
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT
                                                , tell
                                                )
import qualified Discovery.RestDescription.Schema
                                               as Desc
import qualified JSON
import           Generator.Types
import           Generator.Util
import           Generator.Schema.Types

createData
  :: MonadThrow m => ModuleName -> RecordName -> Desc.Schema -> GenData m Text
createData moduleName recName schema = do
  let name = fromMaybe recName $ Desc.schemaId schema
      desc = Desc.schemaDescription schema
  schemaType <- get Desc.schemaType "schema type" schema
  case schemaType of
    (Desc.ObjectType obj  ) -> createObject moduleName name desc obj
    (Desc.ArrayType  array) -> createArray moduleName name array schema
    Desc.AnyType            -> createAnyRecord name desc
    (Desc.StringType  _)    -> createString moduleName name desc schema
    (Desc.IntegerType _)    -> do
      tell [DataImport ImportPrelude]
      pure $ createPrimitive name desc "RIO.Int"
    (Desc.NumberType _) -> do
      tell [DataImport ImportPrelude]
      pure $ createPrimitive name desc "RIO.Float"
    Desc.BooleanType -> do
      tell [DataImport ImportPrelude]
      pure $ createPrimitive name desc "RIO.Bool"
    _ -> undefined

createObject
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.Object
  -> GenData m Text
createObject moduleName name desc obj = do
  tell [DataImport ImportPrelude]
  props    <- createObjectProperties moduleName name desc obj
  addProps <- createObjectAdditionalProperties moduleName name desc obj
  maybe
    (throwM $ GeneratorException
      "faild to get JSON object properties nor additionalProperties"
    )
    pure
    (props <|> addProps)

createObjectProperties
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.Object
  -> GenData m (Maybe Text)
createObjectProperties moduleName name desc obj =
  case Desc.objectProperties obj of
    (Just props) -> createObjectPropertiesContent moduleName name desc props
    _            -> pure Nothing

createObjectPropertiesContent
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.ObjectProperties
  -> GenData m (Maybe Text)
createObjectPropertiesContent moduleName name desc props = if Map.null props
  then do
    let field  = "    un" <> name <> " :: Map RIO.Text Aeson.Value"
        record = createRecordContent name field 1 desc True
    pure . pure $ record
  else do
    field <- createField moduleName name props
    let record = createRecordContent name field (Map.size props) desc False
        aeson  = createAesonContent moduleName name props
    pure . pure $ record <> "\n\n" <> aeson

createObjectAdditionalProperties
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.Object
  -> GenData m (Maybe Text)
createObjectAdditionalProperties moduleName name desc obj =
  case Desc.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema schema)) ->
      createObjectAdditionalPropertiesContent moduleName name desc schema
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> createObjectAdditionalPropertiesContent
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

createObjectAdditionalPropertiesContent
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Maybe Desc
  -> JSON.Schema
  -> GenData m (Maybe Text)
createObjectAdditionalPropertiesContent moduleName name desc schema = do
  fieldType <- createType moduleName (name <> "Value") schema True
  let fieldDesc = descContent 4 (JSON.schemaDescription schema)
      field     = "    un" <> name <> " :: Map RIO.Text " <> fieldType
      record    = createRecordContent name (fieldDesc <> field) 1 desc True
  pure . pure $ record

createArray
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Desc.Array
  -> Desc.Schema
  -> GenData m Text
createArray moduleName name array schema = do
  tell [DataImport ImportPrelude]
  createArrayRecord moduleName name schema array

createArrayRecord
  :: MonadThrow m
  => ModuleName
  -> SchemaName
  -> Desc.Schema
  -> Desc.Array
  -> GenData m Text
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

createAnyRecord :: MonadThrow m => RecordName -> Maybe Desc -> GenData m Text
createAnyRecord name desc =
  pure $ descContent 0 desc <> "type " <> name <> " = Aeson.Value"

createString
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Maybe Desc
  -> Desc.Schema
  -> GenData m Text
createString moduleName name desc schema = case Desc.schemaEnum schema of
  (Just descEnum) -> do
    let descs = fromMaybe (L.repeat "") $ Desc.schemaEnumDescriptions schema
        enums = zip descEnum descs
    tell
      [ DataImport ImportPrelude
      , DataImport ImportGenerics
      , DataImport ImportEnum
      ]
    let content = createFieldEnumContent name enums
        aeson   = createFieldEnumAesonContent moduleName name enums
    pure $ descContent 0 desc <> content <> "\n\n" <> aeson
  _ -> do
    tell [DataImport ImportPrelude]
    pure $ createPrimitive name desc "RIO.Text"

createPrimitive :: RecordName -> Maybe Desc -> Text -> Text
createPrimitive name desc type_ =
  descContent 0 desc <> "type " <> name <> " = " <> type_

createBootData :: MonadThrow m => Desc.Schema -> m Text
createBootData schema = do
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
  :: MonadThrow m
  => ModuleName
  -> RecordName
  -> Desc.ObjectProperties
  -> GenData m Text
createField moduleName name props = do
  fields <- Map.foldrWithKey cons (pure mempty) props
  pure $ T.intercalate ",\n\n" fields
 where
  cons s schema acc = do
    let camelName = name <> toCamelName s
        fieldName = unTitle camelName
        desc      = descContent 4 $ JSON.schemaDescription schema
    fieldType <- createType moduleName camelName schema False
    let field = desc <> "    " <> fieldName <> " :: " <> fieldType
    (field :) <$> acc

createType
  :: MonadThrow m
  => ModuleName
  -> SchemaName
  -> JSON.Schema
  -> Required
  -> GenData m Text
createType moduleName name schema required = do
  jsonType <- get JSON.schemaType "schemaType" schema
  _type    <- case jsonType of
    (JSON.StringType  _) -> createEnumType "RIO.Text" name schema
    (JSON.IntegerType _) -> pure "RIO.Int"
    (JSON.NumberType  _) -> pure "RIO.Float"
    (JSON.ObjectType  _) -> do
      tell [DataSchema (name, schema)]
      pure (moduleName <> "." <> name)
    (JSON.RefType ref) ->
      if Just ref /= L.headMaybe (reverse (T.split (== '.') moduleName)) -- TODO: ここで RecordName が欲しい
        then do
          tell [DataImport (Import ref)]
          pure $ ref <> "." <> ref
        else pure ref
    (JSON.ArrayType array) -> createArrayType moduleName name schema array
    JSON.BooleanType       -> pure "RIO.Bool"
    JSON.AnyType           -> pure "Aeson.Value"
    JSON.NullType          -> undefined
  pure $ if required then _type else "Maybe " <> _type

createEnumType
  :: MonadThrow m => Text -> SchemaName -> JSON.Schema -> GenData m Text
createEnumType defaultType name schema = case JSON.schemaEnum schema of
  (Just jsonEnum) -> do
    let descs = fromMaybe (L.repeat "") $ JSON.schemaEnumDescriptions schema
    tell
      [ DataEnum (name, zip jsonEnum descs)
      , DataImport ImportGenerics
      , DataImport ImportEnum
      ]
    pure name
  _ -> pure defaultType

createArrayType
  :: MonadThrow m
  => ModuleName
  -> SchemaName
  -> JSON.Schema
  -> JSON.Array
  -> GenData m Text
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

createRecordContent :: RecordName -> Text -> Int -> Maybe Text -> Bool -> Text
createRecordContent name field numFields desc addDeriving =
  descContent 0 desc
    <> (if numFields == 1 then "newtype " else "data ")
    <> name
    <> " = "
    <> name
    <> (if numFields == 0 then "" else "\n  {\n" <> field <> "\n  }")
    <> (if addDeriving then " deriving (Aeson.ToJSON, Aeson.FromJSON)" else "")

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

createFieldData :: MonadThrow m => ModuleName -> [Data] -> GenImport m Text
createFieldData moduleName = fmap unLines . foldr f (pure mempty)
 where
  f :: MonadThrow m => Data -> GenImport m [Text] -> GenImport m [Text]
  f (DataSchema schema) acc = do
    (a, schemas) <- lift $ runWriterT $ createFieldDatum moduleName schema
    if null schemas
      then (a :) <$> acc
      else do
        b <- createFieldData moduleName schemas
        (a :) <$> ((b :) <$> acc)
  f (DataEnum (name, enums)) acc = do
    let a     = createFieldEnumContent name enums
        aeson = createFieldEnumAesonContent moduleName name enums
    ((a <> "\n\n" <> aeson) :) <$> acc
  f (DataImport ref) acc = do
    tell $ Set.singleton ref
    acc

createFieldEnumContent :: SchemaName -> EnumList -> Text
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

createFieldEnumAesonContent :: ModuleName -> SchemaName -> EnumList -> Text
createFieldEnumAesonContent moduleName name enums = T.intercalate
  "\n\n"
  [ createFieldEnumConstructorTagModifier name
  , createFieldEnumConstructorTagModifierValues name enums
  , createFieldEnumFromJSONContent moduleName name
  , createFieldEnumToJSONContent moduleName name
  ]

createFieldEnumConstructorTagModifier :: SchemaName -> Text
createFieldEnumConstructorTagModifier name = T.intercalate
  "\n"
  [ fn <> " :: String -> String"
  , fn <> " s = fromMaybe s $ Map.lookup s " <> fn <> "Map"
  ]
  where fn = "to" <> name

createFieldEnumConstructorTagModifierValues :: SchemaName -> EnumList -> Text
createFieldEnumConstructorTagModifierValues name enums = T.intercalate
  "\n"
  [ fn <> " :: Map String String"
  , fn
  <> " =\n  Map.fromList\n    ["
  <> T.intercalate
       "\n    ,"
       (fmap
         (\(e, _) ->
           " (\""
             <> name
             <> toCamelName (T.toLower e)
             <> "\""
             <> ", "
             <> "\""
             <> e
             <> "\")"
         )
         enums
       )
  <> "\n    ]"
  ]
  where fn = "to" <> name <> "Map"

createFieldEnumFromJSONContent :: ModuleName -> SchemaName -> Text
createFieldEnumFromJSONContent moduleName name =
  "instance Aeson.FromJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n"
    <> "  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.constructorTagModifier = to"
    <> name
    <> " }"

createFieldEnumToJSONContent :: ModuleName -> SchemaName -> Text
createFieldEnumToJSONContent moduleName name =
  "instance Aeson.ToJSON "
    <> moduleName
    <> "."
    <> name
    <> " where\n"
    <> "  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.constructorTagModifier = to"
    <> name
    <> " }"

createFieldDatum :: MonadThrow m => ModuleName -> Schema -> GenData m Text
createFieldDatum moduleName (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> do
    let desc = JSON.schemaDescription schema
    fields <- createFieldDatumFields moduleName name desc obj
    field  <- createFieldDatumField moduleName name desc obj
    maybe
      (throwM
        (GeneratorException
          "faild to get JSON object properties nor additionalProperties"
        )
      )
      pure
      (fields <|> field)
  (Just _) -> undefined
  Nothing  -> undefined

createFieldDatumFields
  :: MonadThrow m
  => ModuleName
  -> SchemaName
  -> Maybe Desc
  -> JSON.Object
  -> GenData m (Maybe Text)
createFieldDatumFields moduleName name desc obj =
  case JSON.objectProperties obj of
    (Just props) -> createObjectPropertiesContent moduleName name desc props
    Nothing      -> pure Nothing

createFieldDatumField
  :: MonadThrow m
  => ModuleName
  -> SchemaName
  -> Maybe Desc
  -> JSON.Object
  -> GenData m (Maybe Text)
createFieldDatumField moduleName name desc obj =
  case JSON.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema schema)) ->
      createObjectAdditionalPropertiesContent moduleName name desc schema
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing
