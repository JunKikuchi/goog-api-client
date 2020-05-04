{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module JSON.Schema.Types where

import           RIO                     hiding ( String )
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
  { schemaType        :: Maybe Type
  , schemaTitle       :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaExamples    :: Maybe [Aeson.Value]
  , schemaComment     :: Maybe Text
  , schemaEnum        :: Maybe [Aeson.Value]
  , schemaConst       :: Maybe Aeson.Value
  } deriving (Show, Eq)

instance FromJSON Schema where
  parseJSON = Aeson.withObject "Schema" $ \v -> Schema
    <$> parseType v
    <*> v .:? "title"
    <*> v .:? "description"
    <*> v .:? "examples"
    <*> v .:? "comment"
    <*> v .:? "enum"
    <*> v .:? "const"
    where
      parseType v = do
        t <- v .:? "type" :: Aeson.Parser (Maybe Text)
        case t of
          (Just "string" ) -> (Just . StringType ) <$> parseString  v
          (Just "integer") -> (Just . NumericType) <$> parseNumeric v
          (Just "number" ) -> (Just . NumericType) <$> parseNumeric v
          (Just "object" ) -> (Just . ObjectType ) <$> parseObject  v
          _ -> pure Nothing

data Type
  = StringType String
  | NumericType Numeric
  | ObjectType Object
  | ArrayType Array
  | BooleanType Bool
  | NullType
  deriving (Show, Eq)

type Pattern = Text

data String
  = String
  { stringMinLength :: Maybe Int
  , stringMaxLength :: Maybe Int
  , stringPattern   :: Maybe Pattern
  , stringFormat    :: Maybe Format
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
data Format
  = Int32
  | UInt32
  | Int64
  | UInt64
  | Byte
  | Float
  | Double
  | DateTime
  | Date
 deriving (Show, Eq)

instance FromJSON Format where
  parseJSON = Aeson.withText "Format" $ \case
    "int32"                 -> pure Int32
    "int64"                 -> pure Int64
    "uint32"                -> pure UInt32
    "uint64"                -> pure UInt64
    "byte"                  -> pure Byte
    "float"                 -> pure Float
    "double"                -> pure Double
    "date-time"             -> pure DateTime
    "date"                  -> pure Date
    _                       -> mempty

data Numeric
  = Numeric
  { numericType             :: NumericType
  , numericMultipleOf       :: Maybe Int
  , numericMinimum          :: Maybe Int
  , numericMaximum          :: Maybe Int
  , numericExclusiveMinimum :: Maybe Int
  , numericExclusiveMaximum :: Maybe Int
  } deriving (Show, Eq)

parseNumeric :: Aeson.Object -> Aeson.Parser Numeric
parseNumeric v =
  Numeric
    <$> v
    .:  "type"
    <*> v
    .:? "multipleOf"
    <*> v
    .:? "minimum"
    <*> v
    .:? "maximum"
    <*> v
    .:? "exclusiveMinimum"
    <*> v
    .:? "exclusiveMaximum"

data NumericType
  = Integer
  | Number
  deriving (Show, Eq)

instance FromJSON NumericType where
  parseJSON = Aeson.withText "NumericType" $ \case
    "integer" -> pure Integer
    "number"  -> pure Number
    _         -> mempty

data Object
  = Object
  { objectProperties           :: Maybe (Map Text Schema)
  , objectAdditionalProperties :: Maybe AdditionalProperties
  , objectRequired             :: Maybe [Text]
  , objectPropertyNames        :: Maybe PropertyNames
  , objectMinProperties        :: Maybe Int
  , objectMaxProperties        :: Maybe Int
  , objectDependencies         :: Maybe (Map Text Dependencies)
  , objectPatternProperties    :: Maybe (Map Pattern Dependencies)
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
      parseBool   = Aeson.withBool "AdditionalPropertiesBool" (pure . AdditionalPropertiesBool)
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
  parseJSON s = parseList s <|> parseObject s
    where
      parseList   = Aeson.withObject "DependenciesList"   $ \v -> DependenciesList   <$> v .: "dependencies"
      parseObject = Aeson.withObject "DependenciesObject" $ \v -> DependenciesObject <$> v .: "dependencies"

data Array
  = Array
  { arrayItems           :: Maybe ArrayItems
  , arrayContains        :: Maybe Schema
  , arrayAdditionalItems :: Maybe ArrayAdditionalItems
  , arrayMinItems        :: Maybe Int
  , arrayMaxItems        :: Maybe Int
  , arrayUniqueItems     :: Maybe Bool
  } deriving (Show, Eq)

data ArrayItems
  = ArrayItemsItem Schema
  | ArrayItemsTuple [Schema]
  deriving (Show, Eq)

data ArrayAdditionalItems
  = ArrayAdditionalItemsBool Bool
  | ArrayAdditionalItemsSchema Schema
  deriving (Show, Eq)
