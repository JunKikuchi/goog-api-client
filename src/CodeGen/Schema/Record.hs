{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema.Record where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT
                                                , tell
                                                )
import           Discovery.RestDescription.Schema
                                               as Desc
import qualified JSON.Schema                   as JSON
import           CodeGen.Types
import           CodeGen.Util

createRecord :: Desc.Schema -> GenRecord Text
createRecord schema = case schemaType schema of
  (Just (ObjectType obj)) -> do
    name  <- lift $ get schemaId "schema id" schema
    props <- lift $ get objectProperties "object properties" obj
    field <- createField name props
    let desc = schemaDescription schema
    pure $ createRecordContent name field (Map.size props) desc
  _ -> undefined

createBootRecord :: Desc.Schema -> IO Text
createBootRecord schema = case schemaType schema of
  (Just (ObjectType _)) -> do
    name <- get schemaId "schema id" schema
    pure $ "data " <> name
  _ -> undefined

createField :: RecordName -> ObjectProperties -> GenRecord Text
createField name props = do
  fields <- Map.foldrWithKey cons (pure []) props
  pure $ T.intercalate "\n  , " fields
 where
  cons s schema acc = do
    let camelName = T.concat . fmap toTitle . T.split (== '_') $ s
        fieldName = unTitle name <> toTitle camelName
    fieldType <- createType (toTitle fieldName) schema
    let field = fieldName <> " :: " <> fieldType
    (field :) <$> acc

createType :: SchemaName -> JSON.Schema -> GenRecord Text
createType name schema = do
  jsonType <- get JSON.schemaType "schemaType" schema
  case jsonType of
    (JSON.StringType  _    ) -> tell [GenRef RefPrelude] >> pure "Text"
    (JSON.IntegerType _    ) -> tell [GenRef RefPrelude] >> pure "Int"
    (JSON.NumberType  _    ) -> tell [GenRef RefPrelude] >> pure "Float"
    (JSON.ObjectType  _    ) -> tell [Gen (name, schema)] >> pure name
    (JSON.RefType     ref  ) -> tell [GenRef (Ref ref)] >> pure ref
    (JSON.ArrayType   array) -> createArrayType name array
    JSON.BooleanType         -> tell [GenRef RefPrelude] >> pure "Bool"
    JSON.AnyType             -> tell [GenRef RefGAC] >> pure "GAC.Any"
    JSON.NullType            -> undefined

createArrayType :: SchemaName -> JSON.Array -> GenRecord Text
createArrayType name array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem schema)) -> do
    fieldType <- createType name schema
    pure $ "[" <> fieldType <> "]"
  _ -> undefined

createRecordContent :: RecordName -> Text -> Int -> Maybe Text -> Text
createRecordContent name field size desc =
  maybe "" (\s -> "-- " <> s <> "\n") desc
    <> (if size == 1 then "newtype " else "data ")
    <> name
    <> " = "
    <> name
    <> (if size == 0 then "" else "\n  { " <> field <> "\n  }")

createFieldRecords :: [Gen] -> GenRef Text
createFieldRecords = fmap unLines . foldr f (pure [])
 where
  f :: Gen -> GenRef [Text] -> GenRef [Text]
  f (GenRef ref) acc = do
    tell $ Set.singleton ref
    acc
  f (Gen schema) acc = do
    (a, schemas) <- lift $ runWriterT $ createFieldRecord schema
    if null schemas
      then (a :) <$> acc
      else do
        b <- createFieldRecords schemas
        (a :) <$> ((b :) <$> acc)

createFieldRecord :: CodeGen.Types.Schema -> GenRecord Text
createFieldRecord obj = do
  fields <- createFieldRecordFields obj
  field  <- createFieldRecordField obj
  maybe (error "faild to get JSON object properties nor additionalProperties")
        pure
        (fields <|> field)

createFieldRecordFields :: CodeGen.Types.Schema -> GenRecord (Maybe Text)
createFieldRecordFields (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> case JSON.objectProperties obj of
    (Just props) -> do
      field <- createField name props
      let desc = JSON.schemaDescription schema
      pure . pure $ createRecordContent name field (Map.size props) desc
    Nothing -> pure Nothing
  (Just _) -> undefined
  Nothing  -> pure Nothing

createFieldRecordField :: CodeGen.Types.Schema -> GenRecord (Maybe Text)
createFieldRecordField (name, schema) = case JSON.schemaType schema of
  (Just (JSON.ObjectType obj)) -> case JSON.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema schema')) -> do
      fieldType <- createType name schema'
      let field = T.concat ["un", name] <> " :: Map Text " <> fieldType
      tell [GenRef RefPrelude]
      let desc = JSON.schemaDescription schema'
      pure . pure $ createRecordContent name field 1 desc
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing
  (Just _) -> undefined
  Nothing  -> pure Nothing
