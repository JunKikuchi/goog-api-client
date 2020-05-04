{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module JSON.Schema.Types where

import           RIO                     hiding ( String )
import           Data.Aeson                     ( FromJSON(..)
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
          (Just "string") -> (Just . StringType) <$> parseString v
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

data NumericType
  = Integer
  | Number
  deriving (Show, Eq)

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

data AdditionalProperties
  = AdditionalPropertiesBool Bool
  | AdditionalPropertiesSchema Schema
  deriving (Show, Eq)

newtype PropertyNames
  = PropertyNames
  { propertyNamesPattern :: Pattern
  } deriving (Show, Eq)

data Dependencies
  = DependenciesList [Text]
  | DependenciesObject Object
  deriving (Show, Eq)

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
