{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema.Number where

import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema_Number :: Spec
spec_Test_JSON_Schema_Number = describe "number" $ do
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
