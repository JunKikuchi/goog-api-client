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

createRecord :: Desc.Schema -> GenRecord Text
createRecord schema = case Desc.schemaType schema of
  (Just (Desc.ObjectType obj)) -> do
    name  <- lift $ get Desc.schemaId "schema id" schema
    props <- lift $ get Desc.objectProperties "object properties" obj
    field <- createField name props
    let desc   = Desc.schemaDescription schema
        record = createRecordContent name field (Map.size props) desc
        aeson  = createAesonContent name props
    pure $ record <> "\n\n" <> aeson
  _ -> undefined

createBootRecord :: Desc.Schema -> IO Text
createBootRecord schema = case Desc.schemaType schema of
  (Just (Desc.ObjectType _)) -> do
    name <- get Desc.schemaId "schema id" schema
    pure $ "data " <> name
  _ -> undefined

createField :: RecordName -> Desc.ObjectProperties -> GenRecord Text
createField name props = do
  fields <- Map.foldrWithKey cons (pure []) props
  pure $ T.intercalate ",\n\n" fields
 where
  cons s schema acc = do
    let camelName =
          toTitle
            . T.concat
            . fmap toTitle
            . T.split (\c -> c == '_' || c == '-')
            $ s
        fieldName = unTitle name <> camelName
        desc      = descContent 4 $ JSON.schemaDescription schema
    fieldType <- createType camelName schema False
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

createType :: SchemaName -> JSON.Schema -> Required -> GenRecord Text
createType name schema required = do
  jsonType <- get JSON.schemaType "schemaType" schema
  _type    <- case jsonType of
    (JSON.StringType  _    ) -> createEnumType "Text" name schema
    (JSON.IntegerType _    ) -> tell [GenRef RefPrelude] >> pure "Int"
    (JSON.NumberType  _    ) -> tell [GenRef RefPrelude] >> pure "Float"
    (JSON.ObjectType  _    ) -> tell [Gen (name, schema)] >> pure name
    (JSON.RefType     ref  ) -> tell [GenRef (Ref ref)] >> pure ref
    (JSON.ArrayType   array) -> createArrayType name schema array
    JSON.BooleanType         -> tell [GenRef RefPrelude] >> pure "Bool"
    JSON.AnyType             -> tell [GenRef RefGAC] >> pure "GAC.Any"
    JSON.NullType            -> undefined
  if required
    then pure _type
    else do
      tell [GenRef RefPrelude]
      pure $ "Maybe " <> _type

createEnumType :: Text -> SchemaName -> JSON.Schema -> GenRecord Text
createEnumType defaultType name schema = case JSON.schemaEnum schema of
  (Just jsonEnum) -> do
    let descs = fromMaybe [] $ JSON.schemaEnumDescriptions schema
    tell [GenEnum (name, zip jsonEnum descs)]
    pure name
  _ -> tell [GenRef RefPrelude] >> pure defaultType

createArrayType :: SchemaName -> JSON.Schema -> JSON.Array -> GenRecord Text
createArrayType name schema array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem fieldSchema)) -> do
    let desc      = JSON.schemaDescription schema
        enumDescs = JSON.schemaEnumDescriptions schema
    fieldType <- createType
      name
      (fieldSchema { JSON.schemaDescription      = desc
                   , JSON.schemaEnumDescriptions = enumDescs
                   }
      )
      True
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

createAesonContent :: RecordName -> Desc.ObjectProperties -> Text
createAesonContent name props =
  createFromJSONContent name props <> "\n\n" <> createToJSONContent name props

createFromJSONContent :: RecordName -> Desc.ObjectProperties -> Text
createFromJSONContent name props
  | Map.size props == 0
  = "instance Aeson.FromJSON "
    <> name
    <> " where\n  parseJSON = Aeson.withObject \""
    <> name
    <> "\" (\\v -> if null v then pure "
    <> name
    <> " else mempty)"
  | otherwise
  = "instance Aeson.FromJSON "
    <> name
    <> " where\n  parseJSON = Aeson.withObject \""
    <> name
    <> "\" $ \\v -> "
    <> name
    <> "\n    <$> "
    <> T.intercalate "\n    <*> " (Map.foldrWithKey cons [] props)
  where cons s _schema acc = ("v Aeson..:?" <> " \"" <> s <> "\"") : acc

createToJSONContent :: RecordName -> Desc.ObjectProperties -> Text
createToJSONContent name props
  | Map.size props == 0
  = "instance Aeson.ToJSON "
    <> name
    <> " where\n"
    <> "  toJSON "
    <> name
    <> " = Aeson.object []"
  | otherwise
  = "instance Aeson.ToJSON "
    <> name
    <> " where\n  toJSON(\n    "
    <> name
    <> "\n      "
    <> args
    <> "\n    ) = Aeson.object\n    [ "
    <> obj
    <> "\n    ]"
 where
  names =
    (\key -> (key, unTitle name <> toTitle key <> "'")) <$> Map.keys props
  args = T.intercalate "\n      " (snd <$> names)
  obj  = T.intercalate
    "\n    , "
    ((\(key, argName) -> "\"" <> key <> "\" Aeson..= " <> argName) <$> names)

createFieldRecords :: [Gen] -> GenRef Text
createFieldRecords = fmap unLines . foldr f (pure [])
 where
  f :: Gen -> GenRef [Text] -> GenRef [Text]
  f (GenRef ref) acc = do
    tell $ Set.singleton ref
    acc
  f (GenEnum (name, enums)) acc = do
    let a = createFieldEnum name enums
    (a :) <$> acc
  f (Gen schema) acc = do
    (a, schemas) <- lift $ runWriterT $ createFieldRecord schema
    if null schemas
      then (a :) <$> acc
      else do
        b <- createFieldRecords schemas
        (a :) <$> ((b :) <$> acc)

createFieldEnum :: SchemaName -> Enums -> Text
createFieldEnum name enums = "data " <> name <> "\n  =\n" <> T.intercalate
  "\n  |\n"
  (fmap (\(e, d) -> descContent 2 (Just d) <> "  " <> e) enums)

createFieldRecord :: Schema -> GenRecord Text
createFieldRecord obj = do
  fields <- createFieldRecordFields obj
  field  <- createFieldRecordField obj
  maybe (error "faild to get JSON object properties nor additionalProperties")
        pure
        (fields <|> field)

createFieldRecordFields :: Schema -> GenRecord (Maybe Text)
createFieldRecordFields (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> case JSON.objectProperties obj of
    (Just props) -> do
      field <- createField name props
      let desc   = JSON.schemaDescription schema
          record = createRecordContent name field (Map.size props) desc
          aeson  = createAesonContent name props
      pure . pure $ record <> "\n\n" <> aeson
    Nothing -> pure Nothing
  (Just _) -> undefined
  Nothing  -> pure Nothing

createFieldRecordField :: Schema -> GenRecord (Maybe Text)
createFieldRecordField (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> case JSON.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema fieldSchema)) -> do
      tell [GenRef RefPrelude]
      let desc = JSON.schemaDescription schema
      fieldType <- createType name fieldSchema True
      let fieldDesc = descContent 4 (JSON.schemaDescription fieldSchema)
      let field =
            "    " <> T.concat ["un", name] <> " :: Map Text " <> fieldType
      pure
        .  pure
        $  createRecordContent name (fieldDesc <> field) 1 desc
        <> " deriving (Aeson.ToJSON, Aeson.FromJSON)"
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing
  (Just _) -> undefined
  Nothing  -> pure Nothing
