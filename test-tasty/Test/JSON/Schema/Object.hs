{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema.Object where

import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema_Object :: Spec
spec_Test_JSON_Schema_Object = describe "object" $ do
  describe "Properties" $ do
    let
      json
        = [r|{ "type": "object", "properties": { "number": { "type": "number" }, "street_name": { "type": "string" }, "street_type": { "type": "string", "enum": ["Street", "Avenue", "Boulevard"] } } }|]
      schema = Schema
        { schemaType             = Just (ObjectType objectType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
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
            Nothing
          )
        ]
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "additionalProperties" $ do
    describe "bool" $ do
      let
        json
          = [r|{ "type": "object", "properties": { "number": { "type": "number" }, "street_name": { "type": "string" }, "street_type": { "type": "string", "enum": ["Street", "Avenue", "Boulevard"] } }, "additionalProperties": false }|]
        schema = Schema
          { schemaType             = Just (ObjectType objectType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
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
              Nothing
            )
          ]
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "schema" $ do
      let
        json
          = [r|{ "type": "object", "properties": { "number": { "type": "number" }, "street_name": { "type": "string" }, "street_type": { "type": "string", "enum": ["Street", "Avenue", "Boulevard"] } }, "additionalProperties":  { "type": "string" } }|]
        schema = Schema
          { schemaType             = Just (ObjectType objectType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        objectType = Object
          { objectProperties           = Just propertiess
          , objectAdditionalProperties = Just additionalPropertiesSchema
          , objectRequired             = Nothing
          , objectPropertyNames        = Nothing
          , objectMinProperties        = Nothing
          , objectMaxProperties        = Nothing
          , objectDependencies         = Nothing
          , objectPatternProperties    = Nothing
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
              Nothing
            )
          ]
        additionalPropertiesSchema = AdditionalPropertiesSchema
          (Schema (Just (StringType (String Nothing Nothing Nothing Nothing)))
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
          )
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Required Properties" $ do
    let
      json
        = [r|{ "type": "object", "properties": { "name": { "type": "string" }, "email": { "type": "string" }, "address": { "type": "string" }, "telephone": { "type": "string" } }, "required": ["name", "email"] }|]
      schema = Schema
        { schemaType             = Just (ObjectType objectType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
      objectType = Object
        { objectProperties           = Just propertiess
        , objectAdditionalProperties = Nothing
        , objectRequired             = Just ["name", "email"]
        , objectPropertyNames        = Nothing
        , objectMinProperties        = Nothing
        , objectMaxProperties        = Nothing
        , objectDependencies         = Nothing
        , objectPatternProperties    = Nothing
        }
      propertiess = Map.fromList
        [ ( "name"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        , ( "email"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        , ( "address"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        , ( "telephone"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        ]
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Property names" $ do
    let
      json
        = [r| { "type": "object", "propertyNames": { "pattern": "^[A-Za-z_][A-Za-z0-9_]*$" } }|]
      schema = Schema
        { schemaType             = Just (ObjectType objectType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
      objectType = Object
        { objectProperties           = Nothing
        , objectAdditionalProperties = Nothing
        , objectRequired             = Nothing
        , objectPropertyNames = Just (PropertyNames "^[A-Za-z_][A-Za-z0-9_]*$")
        , objectMinProperties        = Nothing
        , objectMaxProperties        = Nothing
        , objectDependencies         = Nothing
        , objectPatternProperties    = Nothing
        }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Size" $ do
    let json =
          [r|{ "type": "object", "minProperties": 2, "maxProperties": 3 }|]
        schema = Schema
          { schemaType             = Just (ObjectType objectType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        objectType = Object
          { objectProperties           = Nothing
          , objectAdditionalProperties = Nothing
          , objectRequired             = Nothing
          , objectPropertyNames        = Nothing
          , objectMinProperties        = Just 2
          , objectMaxProperties        = Just 3
          , objectDependencies         = Nothing
          , objectPatternProperties    = Nothing
          }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Dependencies" $ do
    describe "list" $ do
      let
        json
          = [r|{ "type": "object", "properties": { "name": { "type": "string" }, "credit_card": { "type": "number" }, "billing_address": { "type": "string" } }, "required": ["name"], "dependencies": { "credit_card": ["billing_address"] } }|]
        schema = Schema
          { schemaType             = Just (ObjectType objectType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        objectType = Object
          { objectProperties           = Just propertiess
          , objectAdditionalProperties = Nothing
          , objectRequired             = Just ["name"]
          , objectPropertyNames        = Nothing
          , objectMinProperties        = Nothing
          , objectMaxProperties        = Nothing
          , objectDependencies         = Just
            (Map.fromList
              [("credit_card", DependenciesList ["billing_address"])]
            )
          , objectPatternProperties    = Nothing
          }
        propertiess = Map.fromList
          [ ( "name"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          , ( "credit_card"
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
              Nothing
            )
          , ( "billing_address"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          ]
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "Dependencies object" $ do
      let
        json
          = [r|{ "type": "object", "properties": { "name": { "type": "string" }, "credit_card": { "type": "number" } }, "required": ["name"], "dependencies": { "credit_card": { "properties": { "billing_address": { "type": "string" } }, "required": ["billing_address"] } } }|]
        schema = Schema
          { schemaType             = Just (ObjectType objectType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        objectType = Object
          { objectProperties           = Just propertiess
          , objectAdditionalProperties = Nothing
          , objectRequired             = Just ["name"]
          , objectPropertyNames        = Nothing
          , objectMinProperties        = Nothing
          , objectMaxProperties        = Nothing
          , objectDependencies         = Just dependencies
          , objectPatternProperties    = Nothing
          }
        propertiess = Map.fromList
          [ ( "name"
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            )
          , ( "credit_card"
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
              Nothing
            )
          ]
        dependencies = Map.fromList
          [ ( "credit_card"
            , DependenciesObject
              (Object
                (Just
                  (Map.fromList
                    [ ( "billing_address"
                      , Schema
                        (Just
                          (StringType (String Nothing Nothing Nothing Nothing))
                        )
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                      )
                    ]
                  )
                )
                Nothing
                (Just ["billing_address"])
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              )
            )
          ]
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Pattern Properties" $ do
    let
      json
        = [r|{ "type": "object", "patternProperties": { "^S_": { "type": "string" }, "^I_": { "type": "integer" } }, "additionalProperties": false }|]
      schema = Schema
        { schemaType             = Just (ObjectType objectType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
      objectType = Object
        { objectProperties           = Nothing
        , objectAdditionalProperties = Just (AdditionalPropertiesBool False)
        , objectRequired             = Nothing
        , objectPropertyNames        = Nothing
        , objectMinProperties        = Nothing
        , objectMaxProperties        = Nothing
        , objectDependencies         = Nothing
        , objectPatternProperties    = Just properties
        }
      properties = Map.fromList
        [ ( "^S_"
          , Schema
            (Just (StringType (String Nothing Nothing Nothing Nothing)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        , ( "^I_"
          , Schema
            (Just
              (IntegerType
                (Integer Nothing Nothing Nothing Nothing Nothing Nothing)
              )
            )
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
          )
        ]
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
