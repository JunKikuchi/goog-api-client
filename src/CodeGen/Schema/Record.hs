{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema.Record where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT
                                                , tell
                                                )
import           Discovery.RestDescription.Schema
import qualified JSON.Schema                   as JSON
import           CodeGen.Types
import           CodeGen.Util

createRecord :: Schema -> GenRecord Text
createRecord schema = case schemaType schema of
  (Just (ObjectType obj)) -> do
    name  <- lift $ get schemaId "schema id" schema
    props <- lift $ get objectProperties "object properties" obj
    field <- createField name props
    pure $ createRecordContent name field (Map.size props)
  _ -> undefined

createField :: RecordName -> ObjectProperties -> GenRecord Text
createField name props = do
  fields <- Map.foldrWithKey cons (pure []) props
  pure $ T.intercalate "\n  , " fields
 where
  cons s schema acc = do
    let camelName = T.concat . fmap toTitle . T.split (== '_') $ s
        fieldName = unTitle name <> toTitle camelName
    field <- createContent fieldName schema
    (field :) <$> acc
  createContent fieldName schema = do
    fieldType <- createType (toTitle fieldName) schema
    pure $ fieldName <> " :: " <> fieldType

createType :: ObjectName -> JSON.Schema -> GenRecord Text
createType name schema = do
  jsonType <- get JSON.schemaType "schemaType" schema
  case jsonType of
    (JSON.StringType  _    ) -> pure "Text"
    (JSON.IntegerType _    ) -> pure "Int"
    (JSON.NumberType  _    ) -> pure "Float"
    (JSON.ObjectType  obj  ) -> createObjectType name obj
    (JSON.ArrayType   array) -> createArrayType name array
    (JSON.RefType     ref  ) -> createRefType ref
    JSON.BooleanType         -> pure "Bool"
    JSON.AnyType             -> createAnyType
    JSON.NullType            -> undefined

createObjectType :: ObjectName -> JSON.Object -> GenRecord Text
createObjectType name obj = do
  tell [GenObject (name, obj)]
  pure name

createArrayType :: ObjectName -> JSON.Array -> GenRecord Text
createArrayType name array = case JSON.arrayItems array of
  (Just (JSON.ArrayItemsItem schema)) -> do
    fieldType <- createType name schema
    pure $ "[" <> fieldType <> "]"
  _ -> undefined

createRefType :: Text -> GenRecord Text
createRefType ref = do
  tell [GenRef (Ref ref)]
  pure ref

createAnyType :: GenRecord Text
createAnyType = do
  tell [GenRef RefGAC]
  pure "GAC.Any"

createRecordContent :: RecordName -> Text -> Int -> Text
createRecordContent name field size =
  (if size == 1 then "newtype " else "data ")
    <> name
    <> " = "
    <> name
    <> (if size == 0 then "" else "\n  { " <> field <> "\n  }")
    <> " deriving Show"

createFieldRecords :: [Gen] -> GenRef Text
createFieldRecords = fmap (T.intercalate "\n\n") . foldr f (pure [])
 where
  f :: Gen -> GenRef [Text] -> GenRef [Text]
  f (GenRef ref) acc = do
    tell [ref]
    acc
  f (GenObject obj) acc = do
    (a, objs) <- lift $ runWriterT $ createFieldRecord obj
    if null objs
      then (a :) <$> acc
      else do
        b <- createFieldRecords objs
        (a :) <$> ((b :) <$> acc)

createFieldRecord :: CodeGen.Types.Object -> GenRecord Text
createFieldRecord obj = do
  fields <- createFieldRecordFields obj
  field  <- createFieldRecordField obj
  maybe (error "faild to get JSON object properties nor additionalProperties")
        pure
        (fields <|> field)

createFieldRecordFields :: CodeGen.Types.Object -> GenRecord (Maybe Text)
createFieldRecordFields (name, obj) = case JSON.objectProperties obj of
  (Just props) -> do
    field <- createField name props
    pure . pure $ createRecordContent name field (Map.size props)
  Nothing -> pure Nothing

createFieldRecordField :: CodeGen.Types.Object -> GenRecord (Maybe Text)
createFieldRecordField (name, obj) =
  case JSON.objectAdditionalProperties obj of
    (Just (JSON.AdditionalPropertiesSchema schema)) -> do
      fieldType <- createType name schema
      let field = T.concat ["un", name] <> " :: Map Text " <> fieldType
      pure . pure $ createRecordContent name field 1
    (Just (JSON.AdditionalPropertiesBool _)) -> undefined
    Nothing -> pure Nothing
