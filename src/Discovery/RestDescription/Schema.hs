{-# LANGUAGE OverloadedStrings #-}
module Discovery.RestDescription.Schema where

import           RIO                     hiding ( Integer
                                                , String
                                                )
import           Data.Aeson                     ( (.:?) )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import qualified JSON.Schema                   as JSON

data Schema
  = Schema
  { schemaId :: Maybe Text
  , schemaType :: Maybe Type
  , schemaRef :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaDefault :: Maybe Text
  , schemaEnum :: Maybe [Text]
  , schemaEnumDescriptions :: Maybe [Text]
  , schemaRepeated :: Maybe Bool
  , schemaLocation :: Maybe Text
  , schemaAnnotations :: Maybe SchemaAnnotations
  } deriving Show

instance Aeson.FromJSON Schema where
  parseJSON = Aeson.withObject "Schema" $ \v -> Schema
    <$> v .:? "id"
    <*> parseType v
    <*> v .:? "$ref"
    <*> v .:? "description"
    <*> v .:? "default"
    <*> v .:? "enum"
    <*> v .:? "enumDescriptions"
    <*> v .:? "repeated"
    <*> v .:? "location"
    <*> v .:? "annotations"
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
          (Just "null"   ) -> pure (Just NullType   )
          _ -> pure Nothing

data Type
  = StringType String
  | IntegerType Integer
  | NumberType Number
  | ObjectType Object
  | ArrayType Array
  | BooleanType
  | NullType
  deriving Show

data String
  = String
  { stringPattern :: Maybe JSON.Pattern
  , stringFormat :: Maybe JSON.StringFormat
  } deriving Show

parseString :: Aeson.Object -> Aeson.Parser String
parseString v = String <$> v .:? "pattern" <*> v .:? "format"

data Integer
  = Integer
  { integerMinimum :: Maybe Text
  , integerMaximum :: Maybe Text
  , integerFormat :: Maybe JSON.IntegerFormat
  } deriving Show

parseInteger :: Aeson.Object -> Aeson.Parser Integer
parseInteger v =
  Integer <$> v .:? "minimum" <*> v .:? "maximum" <*> v .:? "format"

data Number
  = Number
  { numberMinimum :: Maybe Text
  , numberMaximum :: Maybe Text
  , numberFormat :: Maybe JSON.NumberFormat
  } deriving Show

parseNumber :: Aeson.Object -> Aeson.Parser Number
parseNumber v =
  Number <$> v .:? "minimum" <*> v .:? "maximum" <*> v .:? "format"

data Object
  = Object
  { objectProperties :: Maybe (Map Text Schema)
  , objectAdditionalProperties :: Maybe JSON.AdditionalProperties
  , objectRequired :: Maybe Bool
  } deriving Show

parseObject :: Aeson.Object -> Aeson.Parser Object
parseObject v =
  Object
    <$> v
    .:? "properties"
    <*> v
    .:? "additionalProperties"
    <*> v
    .:? "required"

newtype Array
  = Array
  { arrayItems :: Maybe JSON.ArrayItems
  } deriving Show

parseArray :: Aeson.Object -> Aeson.Parser Array
parseArray v = Array <$> v .:? "items"

newtype SchemaAnnotations
  = SchemaAnnotations
  { schemaAnnotationsRequired :: Maybe [Text]
  } deriving Show

instance Aeson.FromJSON SchemaAnnotations where
  parseJSON = Aeson.withObject "SchemaAnnotations" $ \v -> SchemaAnnotations
    <$> v .:? "required"
