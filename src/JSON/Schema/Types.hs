module JSON.Schema.Types where

import           RIO                            ( Bool
                                                , Int
                                                , Map
                                                , Maybe
                                                , Text
                                                )
import           Data.Aeson                     ( Value )

-- https://json-schema.org/understanding-json-schema/index.html

data Schema
  = Schema
  { schemaType        :: Maybe Type
  , schemaTitle       :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaExamples    :: Maybe [Value]
  , schemaComment     :: Maybe Text
  , schemaEnum        :: Maybe [Value]
  , schemaConst       :: Maybe Value
  }

data Type
  = StringType String
  | NumericType Numeric
  | ObjectType Object
  | ArrayType Array
  | BooleanType Bool
  | NullType

type Pattern = Text

data String
  = String
  { stringMinLength :: Maybe Int
  , stringMaxLength :: Maybe Int
  , stringPattern   :: Maybe Pattern
  , stringFormat    :: Maybe Format
  }

data Format
  = DateTime
  | Time
  | Date
  | EMail
  | IDNEMail
  | Hostname
  | IDNHostname
  | IPV4
  | IPV6
  | URL
  | URLReference
  | IRI
  | IRIReference
  | URITemplate
  | JSONPointer
  | RelativeJSONPointer
  | Regex

data Numeric
  = Numeric
  { numericType             :: NumericType
  , numericMultipleOf       :: Maybe Int
  , numericMinimum          :: Maybe Int
  , numericMaximum          :: Maybe Int
  , numericExclusiveMinimum :: Maybe Int
  , numericExclusiveMaximum :: Maybe Int
  }

data NumericType
  = Integer
  | Number

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
  }

data AdditionalProperties
  = AdditionalPropertiesBool Bool
  | AdditionalPropertiesSchema Schema

newtype PropertyNames
  = PropertyNames
  { propertyNamesPattern :: Pattern
  }

data Dependencies
  = DependenciesList [Text]
  | DependenciesObject Object

data Array
  = Array
  { arrayItems           :: Maybe ArrayItems
  , arrayContains        :: Maybe Schema
  , arrayAdditionalItems :: Maybe ArrayAdditionalItems
  , arrayMinItems        :: Maybe Int
  , arrayMaxItems        :: Maybe Int
  , arrayUniqueItems     :: Maybe Bool
  }

data ArrayItems
  = ArrayItemsItem Schema
  | ArrayItemsTuple [Schema]

data ArrayAdditionalItems
  = ArrayAdditionalItemsBool Bool
  | ArrayAdditionalItemsSchema Schema
