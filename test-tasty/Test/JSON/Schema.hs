{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema where

import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema :: Spec
spec_Test_JSON_Schema =
  describe "parse" $ describe "Type-specific keywords" $ do
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
