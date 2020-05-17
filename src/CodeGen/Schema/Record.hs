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
    let desc = Desc.schemaDescription schema
    pure $ createRecordContent name field (Map.size props) desc
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
    let camelName = toTitle . T.concat . fmap toTitle . T.split (== '_') $ s
        fieldName = unTitle name <> camelName
        desc      = descContent 0 $ JSON.schemaDescription schema
    fieldType <- createType camelName schema
    let field = fieldName <> " :: " <> fieldType
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

createType :: SchemaName -> JSON.Schema -> GenRecord Text
createType name schema = do
  jsonType <- get JSON.schemaType "schemaType" schema
  case jsonType of
    (JSON.StringType  _    ) -> createEnumType "Text" name schema
    (JSON.IntegerType _    ) -> tell [GenRef RefPrelude] >> pure "Int"
    (JSON.NumberType  _    ) -> tell [GenRef RefPrelude] >> pure "Float"
    (JSON.ObjectType  _    ) -> tell [Gen (name, schema)] >> pure name
    (JSON.RefType     ref  ) -> tell [GenRef (Ref ref)] >> pure ref
    (JSON.ArrayType   array) -> createArrayType name schema array
    JSON.BooleanType         -> tell [GenRef RefPrelude] >> pure "Bool"
    JSON.AnyType             -> tell [GenRef RefGAC] >> pure "GAC.Any"
    JSON.NullType            -> undefined

createEnumType :: Text -> SchemaName -> JSON.Schema -> GenRecord Text
createEnumType defaultType name schema = case JSON.schemaEnum schema of
  (Just jsonEnum) -> do
    let descs = fromMaybe [] $ JSON.schemaEnumDescriptions schema
    tell [GenEnum (name, zip jsonEnum descs) schema]
    pure name
  _ -> tell [GenRef RefPrelude] >> pure defaultType

createArrayType :: SchemaName -> JSON.Schema -> JSON.Array -> GenRecord Text
createArrayType name schema array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem fieldSchema)) -> do
    let desc = JSON.schemaDescription schema
    fieldType <- createType name (fieldSchema { JSON.schemaDescription = desc })
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

createFieldRecords :: [Gen] -> GenRef Text
createFieldRecords = fmap unLines . foldr f (pure [])
 where
  f :: Gen -> GenRef [Text] -> GenRef [Text]
  f (GenRef ref) acc = do
    tell $ Set.singleton ref
    acc
  f (GenEnum (name, enums) schema) acc = do
    let a = createFieldEnum name enums schema
    (a :) <$> acc
  f (Gen schema) acc = do
    (a, schemas) <- lift $ runWriterT $ createFieldRecord schema
    if null schemas
      then (a :) <$> acc
      else do
        b <- createFieldRecords schemas
        (a :) <$> ((b :) <$> acc)

createFieldEnum :: SchemaName -> Enums -> JSON.Schema -> Text
createFieldEnum name enums schema =
  desc <> "data " <> name <> "\n  =\n" <> T.intercalate
    "\n  |\n"
    (fmap (\(e, d) -> descContent 2 (Just d) <> "  " <> e) enums)
  where desc = descContent 0 $ JSON.schemaDescription schema

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
      let desc = JSON.schemaDescription schema
      pure . pure $ createRecordContent name field (Map.size props) desc
    Nothing -> pure Nothing
  (Just _) -> undefined
  Nothing  -> pure Nothing

createFieldRecordField :: Schema -> GenRecord (Maybe Text)
createFieldRecordField (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> case JSON.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema fieldSchema)) -> do
      tell [GenRef RefPrelude]
      let desc = JSON.schemaDescription schema

      fieldType <- createType name fieldSchema
      let fieldDesc = descContent 0 (JSON.schemaDescription fieldSchema)
      let field     = T.concat ["un", name] <> " :: Map Text " <> fieldType

      pure . pure $ createRecordContent name (fieldDesc <> field) 1 desc
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing
  (Just _) -> undefined
  Nothing  -> pure Nothing
