{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module JSON.Schema where

import           RIO                     hiding ( Integer
                                                , String
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , (.:)
                                                , (.:?)
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson

-- https://json-schema.org/understanding-json-schema/index.html
-- https://developers.google.com/discovery/v1/type-format

data Schema
  = Schema
  { schemaType :: Maybe Type
  , schemaTitle :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaExamples :: Maybe [Aeson.Value]
  , schemaComment :: Maybe Text
  , schemaEnum :: Maybe [Text]
  , schemaEnumDescriptions :: Maybe [Text]
  , schemaConst :: Maybe Aeson.Value
  } deriving (Show, Eq)

instance FromJSON Schema where
  parseJSON = Aeson.withObject "Schema" $ \v -> Schema
    <$> parseType v
    <*> v .:? "title"
    <*> v .:? "description"
    <*> v .:? "examples"
    <*> v .:? "comment"
    <*> v .:? "enum"
    <*> v .:? "enumDescriptions"
    <*> v .:? "const"
    where
      parseType v = do
        t <- v .:? "type" :: Aeson.Parser (Maybe Text)
        case t of
          (Just "string" ) -> (Just . StringType ) <$> parseString  v
          (Just "integer") -> (Just . IntegerType) <$> parseInteger v
          (Just "number" ) -> (Just . NumberType ) <$> parseNumber  v
          (Just "object" ) -> (Just . ObjectType ) <$> parseObject  v
          (Just "array"  ) -> (Just . ArrayType  ) <$> parseArray   v
          (Just "boolean") -> pure (Just BooleanType)
          (Just "any"    ) -> pure (Just AnyType    )
          (Just "null"   ) -> pure (Just NullType   )
          _ -> do
            ref <- v .:? "$ref" :: Aeson.Parser (Maybe Text)
            pure $ fmap RefType ref

data Type
  = StringType String
  | IntegerType Integer
  | NumberType Number
  | ObjectType Object
  | ArrayType Array
  | BooleanType
  | AnyType
  | NullType
  | RefType Text
  deriving (Show, Eq)

type Pattern = Text

data String
  = String
  { stringMinLength :: Maybe Int
  , stringMaxLength :: Maybe Int
  , stringPattern :: Maybe Pattern
  , stringFormat :: Maybe StringFormat
  } deriving (Show, Eq)

parseString :: Aeson.Object -> Aeson.Parser String
parseString v =
  String
    <$> v
    .:? "minLength"
    <*> v
    .:? "maxLength"
    <*> v
    .:? "pattern"
    <*> v
    .:? "format"

-- https://developers.google.com/discovery/v1/type-format
data StringFormat
  = Byte
  | Date
  | DateTime
  | Int64
  | UInt64
  | GoogleDateTime
  | GoogleDuration
  | GoogleFieldmask
  deriving (Show, Eq)

instance FromJSON StringFormat where
  parseJSON = Aeson.withText "StringFormat" $ \case
    "byte"             -> pure Byte
    "date"             -> pure Date
    "date-time"        -> pure DateTime
    "int64"            -> pure Int64
    "uint64"           -> pure UInt64
    "google-datetime"  -> pure GoogleDateTime
    "google-duration"  -> pure GoogleDuration
    "google-fieldmask" -> pure GoogleFieldmask
    _                 -> mempty

data Integer
  = Integer
  { integerMultipleOf :: Maybe Int
  , integerMinimum :: Maybe Text
  , integerMaximum :: Maybe Text
  , integerExclusiveMinimum :: Maybe Bool
  , integerExclusiveMaximum :: Maybe Bool
  , integerFormat :: Maybe IntegerFormat
  } deriving (Show, Eq)

parseInteger :: Aeson.Object -> Aeson.Parser Integer
parseInteger v =
  Integer
    <$> v
    .:? "multipleOf"
    <*> v
    .:? "minimum"
    <*> v
    .:? "maximum"
    <*> v
    .:? "exclusiveMinimum"
    <*> v
    .:? "exclusiveMaximum"
    <*> v
    .:? "format"

data IntegerFormat
  = Int32
  | UInt32
 deriving (Show, Eq)

instance FromJSON IntegerFormat where
  parseJSON = Aeson.withText "IntegerFormat" $ \case
    "int32"  -> pure Int32
    "uint32" -> pure UInt32
    _        -> mempty

data Number
  = Number
  { numberMultipleOf :: Maybe Int
  , numberMinimum :: Maybe Int
  , numberMaximum :: Maybe Int
  , numberExclusiveMinimum :: Maybe Bool
  , numberExclusiveMaximum :: Maybe Bool
  , numberFormat :: Maybe NumberFormat
  } deriving (Show, Eq)

parseNumber :: Aeson.Object -> Aeson.Parser Number
parseNumber v =
  Number
    <$> v
    .:? "multipleOf"
    <*> v
    .:? "minimum"
    <*> v
    .:? "maximum"
    <*> v
    .:? "exclusiveMinimum"
    <*> v
    .:? "exclusiveMaximum"
    <*> v
    .:? "format"

data NumberFormat
  = Double
  | Float
 deriving (Show, Eq)

instance FromJSON NumberFormat where
  parseJSON = Aeson.withText "NumberFormat" $ \case
    "double" -> pure Double
    "float"  -> pure Float
    _        -> mempty

data Object
  = Object
  { objectProperties :: Maybe (Map Text Schema)
  , objectAdditionalProperties :: Maybe AdditionalProperties
  , objectRequired :: Maybe [Text]
  , objectPropertyNames :: Maybe PropertyNames
  , objectMinProperties :: Maybe Int
  , objectMaxProperties :: Maybe Int
  , objectDependencies :: Maybe (Map Text Dependencies)
  , objectPatternProperties :: Maybe (Map Pattern Schema)
  } deriving (Show, Eq)

instance FromJSON Object where
  parseJSON = Aeson.withObject "Object" parseObject

parseObject :: Aeson.Object -> Aeson.Parser Object
parseObject v =
  Object
    <$> v
    .:? "properties"
    <*> v
    .:? "additionalProperties"
    <*> v
    .:? "required"
    <*> v
    .:? "propertyNames"
    <*> v
    .:? "minProperties"
    <*> v
    .:? "maxProperties"
    <*> v
    .:? "dependencies"
    <*> v
    .:? "patternProperties"

data AdditionalProperties
  = AdditionalPropertiesBool Bool
  | AdditionalPropertiesSchema Schema
  deriving (Show, Eq)

instance FromJSON AdditionalProperties where
  parseJSON s = parseBool s <|> parseSchema s
    where
      parseBool   = fmap AdditionalPropertiesBool . parseJSON
      parseSchema = fmap AdditionalPropertiesSchema . parseJSON

newtype PropertyNames
  = PropertyNames
  { propertyNamesPattern :: Pattern
  } deriving (Show, Eq)

instance FromJSON PropertyNames where
  parseJSON = Aeson.withObject "PropertyNames" $ \v ->
    PropertyNames <$> v .: "pattern"

data Dependencies
  = DependenciesList [Text]
  | DependenciesObject Object
  deriving (Show, Eq)

instance FromJSON Dependencies where
  parseJSON s = parseDepList s <|> parseDepObject s
    where
      parseDepList   = fmap DependenciesList . parseJSON
      parseDepObject = fmap DependenciesObject . parseJSON

data Array
  = Array
  { arrayItems :: Maybe ArrayItems
  , arrayContains :: Maybe Schema
  , arrayAdditionalItems :: Maybe ArrayAdditionalItems
  , arrayMinItems :: Maybe Int
  , arrayMaxItems :: Maybe Int
  , arrayUniqueItems :: Maybe Bool
  } deriving (Show, Eq)

parseArray :: Aeson.Object -> Aeson.Parser Array
parseArray v =
  Array
    <$> v
    .:? "items"
    <*> v
    .:? "contains"
    <*> v
    .:? "additionalItems"
    <*> v
    .:? "minItems"
    <*> v
    .:? "maxItems"
    <*> v
    .:? "uniqueItems"

data ArrayItems
  = ArrayItemsItem Schema
  | ArrayItemsTuple [Schema]
  deriving (Show, Eq)

instance FromJSON ArrayItems where
  parseJSON s = parseItem s <|> parseTuple s
    where
      parseItem  = fmap ArrayItemsItem . parseJSON
      parseTuple = fmap ArrayItemsTuple . parseJSON

data ArrayAdditionalItems
  = ArrayAdditionalItemsBool Bool
  | ArrayAdditionalItemsSchema Schema
  deriving (Show, Eq)

instance FromJSON ArrayAdditionalItems where
  parseJSON s = parseBool s <|> parseSchema s
    where
      parseBool   = fmap ArrayAdditionalItemsBool . parseJSON
      parseSchema = fmap ArrayAdditionalItemsSchema . parseJSON
