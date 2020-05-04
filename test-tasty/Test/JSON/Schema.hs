{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema where

import           Prelude                        ( print )
import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema :: Spec
spec_Test_JSON_Schema = describe "parse" $ do
  describe "Type-specific keywords" $ do
    describe "string" $ do
      let json   = [r|{ "type": "string" }|]
          schema = Schema
            { schemaType        = Just (StringType stringType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          stringType = String
            { stringMinLength = Nothing
            , stringMaxLength = Nothing
            , stringPattern   = Nothing
            , stringFormat    = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "integer" $ do
      let json   = [r|{ "type": "integer"}|]
          schema = Schema
            { schemaType        = Just (IntegerType integerType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          integerType = Integer
            { integerMultipleOf       = Nothing
            , integerMinimum          = Nothing
            , integerMaximum          = Nothing
            , integerExclusiveMinimum = Nothing
            , integerExclusiveMaximum = Nothing
            , integerFormat           = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "number" $ do
      let json   = [r|{ "type": "number"}|]
          schema = Schema
            { schemaType        = Just (NumberType numberType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          numberType = Number
            { numberMultipleOf       = Nothing
            , numberMinimum          = Nothing
            , numberMaximum          = Nothing
            , numberExclusiveMinimum = Nothing
            , numberExclusiveMaximum = Nothing
            , numberFormat           = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "object" $ do
      let json   = [r|{ "type": "object"}|]
          schema = Schema
            { schemaType        = Just (ObjectType objectType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          objectType = Object
            { objectProperties           = Nothing
            , objectAdditionalProperties = Nothing
            , objectRequired             = Nothing
            , objectPropertyNames        = Nothing
            , objectMinProperties        = Nothing
            , objectMaxProperties        = Nothing
            , objectDependencies         = Nothing
            , objectPatternProperties    = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "array" $ do
      let json   = [r|{ "type": "array"}|]
          schema = Schema
            { schemaType        = Just (ArrayType arrayType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          arrayType = Array
            { arrayItems           = Nothing
            , arrayContains        = Nothing
            , arrayAdditionalItems = Nothing
            , arrayMinItems        = Nothing
            , arrayMaxItems        = Nothing
            , arrayUniqueItems     = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "boolean" $ do
      let json   = [r|{ "type": "boolean"}|]
          schema = Schema
            { schemaType        = Just BooleanType
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "null" $ do
      let json   = [r|{ "type": "null"}|]
          schema = Schema
            { schemaType        = Just NullType
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "string" $ do
    describe "length" $ do
      let json   = [r|{ "type": "string", "minLength": 2, "maxLength": 3 }|]
          schema = Schema
            { schemaType        = Just (StringType stringType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          stringType = String
            { stringMinLength = Just 2
            , stringMaxLength = Just 3
            , stringPattern   = Nothing
            , stringFormat    = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Regular Expressions" $ do
      let
        json
          = [r|{ "type": "string", "pattern": "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$" }|]
        schema = Schema
          { schemaType        = Just (StringType stringType)
          , schemaTitle       = Nothing
          , schemaDescription = Nothing
          , schemaExamples    = Nothing
          , schemaComment     = Nothing
          , schemaEnum        = Nothing
          , schemaConst       = Nothing
          }
        stringType = String
          { stringMinLength = Nothing
          , stringMaxLength = Nothing
          , stringPattern   = Just "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"
          , stringFormat    = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Format" $ do
      let json   = [r|{ "type": "string", "format": "byte" }|]
          schema = Schema
            { schemaType        = Just (StringType stringType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          stringType = String
            { stringMinLength = Nothing
            , stringMaxLength = Nothing
            , stringPattern   = Nothing
            , stringFormat    = Just Byte
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "integer" $ do
    describe "Multiples" $ do
      let json   = [r|{ "type": "integer", "multipleOf": 10 }|]
          schema = Schema
            { schemaType        = Just (IntegerType integerType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          integerType = Integer
            { integerMultipleOf       = Just 10
            , integerMinimum          = Nothing
            , integerMaximum          = Nothing
            , integerExclusiveMinimum = Nothing
            , integerExclusiveMaximum = Nothing
            , integerFormat           = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Range" $ do
      let
        json
          = [r|{ "type": "integer", "minimum": 0, "maximum": 100, "exclusiveMinimum": true, "exclusiveMaximum": true }|]
        schema = Schema
          { schemaType        = Just (IntegerType integerType)
          , schemaTitle       = Nothing
          , schemaDescription = Nothing
          , schemaExamples    = Nothing
          , schemaComment     = Nothing
          , schemaEnum        = Nothing
          , schemaConst       = Nothing
          }
        integerType = Integer
          { integerMultipleOf       = Nothing
          , integerMinimum          = Just 0
          , integerMaximum          = Just 100
          , integerExclusiveMinimum = Just True
          , integerExclusiveMaximum = Just True
          , integerFormat           = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Format" $ do
      let json   = [r|{ "type": "integer", "format": "int32" }|]
          schema = Schema
            { schemaType        = Just (IntegerType integerType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          integerType = Integer
            { integerMultipleOf       = Nothing
            , integerMinimum          = Nothing
            , integerMaximum          = Nothing
            , integerExclusiveMinimum = Nothing
            , integerExclusiveMaximum = Nothing
            , integerFormat           = Just Int32
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "number" $ do
    describe "Multiples" $ do
      let json   = [r|{ "type": "number", "multipleOf": 10 }|]
          schema = Schema
            { schemaType        = Just (NumberType numberType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          numberType = Number
            { numberMultipleOf       = Just 10
            , numberMinimum          = Nothing
            , numberMaximum          = Nothing
            , numberExclusiveMinimum = Nothing
            , numberExclusiveMaximum = Nothing
            , numberFormat           = Nothing
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Range" $ do
      let
        json
          = [r|{ "type": "number", "minimum": 0, "maximum": 100, "exclusiveMinimum": true, "exclusiveMaximum": true }|]
        schema = Schema
          { schemaType        = Just (NumberType numberType)
          , schemaTitle       = Nothing
          , schemaDescription = Nothing
          , schemaExamples    = Nothing
          , schemaComment     = Nothing
          , schemaEnum        = Nothing
          , schemaConst       = Nothing
          }
        numberType = Number
          { numberMultipleOf       = Nothing
          , numberMinimum          = Just 0
          , numberMaximum          = Just 100
          , numberExclusiveMinimum = Just True
          , numberExclusiveMaximum = Just True
          , numberFormat           = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Format" $ do
      let json   = [r|{ "type": "number", "format": "double" }|]
          schema = Schema
            { schemaType        = Just (NumberType numberType)
            , schemaTitle       = Nothing
            , schemaDescription = Nothing
            , schemaExamples    = Nothing
            , schemaComment     = Nothing
            , schemaEnum        = Nothing
            , schemaConst       = Nothing
            }
          numberType = Number
            { numberMultipleOf       = Nothing
            , numberMinimum          = Nothing
            , numberMaximum          = Nothing
            , numberExclusiveMinimum = Nothing
            , numberExclusiveMaximum = Nothing
            , numberFormat           = Just Double
            }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "object" $ describe "Properties" $ do
    let
      json
        = [r|{ "type": "object", "properties": { "number": { "type": "number" }, "street_name": { "type": "string" }, "street_type": { "type": "string", "enum": ["Street", "Avenue", "Boulevard"]} } }|]
      schema = Schema
        { schemaType        = Just (ObjectType objectType)
        , schemaTitle       = Nothing
        , schemaDescription = Nothing
        , schemaExamples    = Nothing
        , schemaComment     = Nothing
        , schemaEnum        = Nothing
        , schemaConst       = Nothing
        }
      propertiess = Map.fromList
        [ ( "number"
          , Schema
            (Just
              (NumberType
                (Number Nothing Nothing Nothing Nothing Nothing Nothing)
              )
            )
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        , ( "street_name"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        , ( "street_type"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            (Just ["Street", "Avenue", "Boulevard"])
            Nothing
          )
        ]
      objectType = Object
        { objectProperties           = Just propertiess
        , objectAdditionalProperties = Nothing
        , objectRequired             = Nothing
        , objectPropertyNames        = Nothing
        , objectMinProperties        = Nothing
        , objectMaxProperties        = Nothing
        , objectDependencies         = Nothing
        , objectPatternProperties    = Nothing
        }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "additionalProperties bool" $ do
      let
        json
          = [r|{ "type": "object", "properties": { "number": { "type": "number" }, "street_name": { "type": "string" }, "street_type": { "type": "string", "enum": ["Street", "Avenue", "Boulevard"]} }, "additionalProperties": false }|]
        schema = Schema
          { schemaType        = Just (ObjectType objectType)
          , schemaTitle       = Nothing
          , schemaDescription = Nothing
          , schemaExamples    = Nothing
          , schemaComment     = Nothing
          , schemaEnum        = Nothing
          , schemaConst       = Nothing
          }
        propertiess = Map.fromList
          [ ( "number"
            , Schema
              (Just
                (NumberType
                  (Number Nothing Nothing Nothing Nothing Nothing Nothing)
                )
              )
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          , ( "street_name"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          , ( "street_type"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              (Just ["Street", "Avenue", "Boulevard"])
              Nothing
            )
          ]
        objectType = Object
          { objectProperties           = Just propertiess
          , objectAdditionalProperties = Just (AdditionalPropertiesBool False)
          , objectRequired             = Nothing
          , objectPropertyNames        = Nothing
          , objectMinProperties        = Nothing
          , objectMaxProperties        = Nothing
          , objectDependencies         = Nothing
          , objectPatternProperties    = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "additionalProperties schema" $ do
      let
        json
          = [r|{ "type": "object", "properties": { "number": { "type": "number" }, "street_name": { "type": "string" }, "street_type": { "type": "string", "enum": ["Street", "Avenue", "Boulevard"]} }, "additionalProperties":  { "type": "string" } }|]
        schema = Schema
          { schemaType        = Just (ObjectType objectType)
          , schemaTitle       = Nothing
          , schemaDescription = Nothing
          , schemaExamples    = Nothing
          , schemaComment     = Nothing
          , schemaEnum        = Nothing
          , schemaConst       = Nothing
          }
        propertiess = Map.fromList
          [ ( "number"
            , Schema
              (Just
                (NumberType
                  (Number Nothing Nothing Nothing Nothing Nothing Nothing)
                )
              )
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          , ( "street_name"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          , ( "street_type"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              (Just ["Street", "Avenue", "Boulevard"])
              Nothing
            )
          ]
        objectType = Object
          { objectProperties           = Just propertiess
          , objectAdditionalProperties = Just
            (AdditionalPropertiesSchema
              (Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              )
            )
          , objectRequired             = Nothing
          , objectPropertyNames        = Nothing
          , objectMinProperties        = Nothing
          , objectMaxProperties        = Nothing
          , objectDependencies         = Nothing
          , objectPatternProperties    = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema

