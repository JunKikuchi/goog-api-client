{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema.Integer where

import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema_Integer :: Spec
spec_Test_JSON_Schema_Integer = describe "integer" $ do
  describe "Multiples" $ do
    let json   = [r|{ "type": "integer", "multipleOf": 10 }|]
        schema = Schema
          { schemaType             = Just (IntegerType integerType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
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
        = [r|{ "type": "integer", "minimum": "0", "maximum": "100", "exclusiveMinimum": true, "exclusiveMaximum": true }|]
      schema = Schema
        { schemaType             = Just (IntegerType integerType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
      integerType = Integer
        { integerMultipleOf       = Nothing
        , integerMinimum          = Just "0"
        , integerMaximum          = Just "100"
        , integerExclusiveMinimum = Just True
        , integerExclusiveMaximum = Just True
        , integerFormat           = Nothing
        }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Format" $ do
    let json   = [r|{ "type": "integer", "format": "int32" }|]
        schema = Schema
          { schemaType             = Just (IntegerType integerType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
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
