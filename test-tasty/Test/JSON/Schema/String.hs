{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema.String where

import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema_String :: Spec
spec_Test_JSON_Schema_String = describe "string" $ do
  describe "length" $ do
    let json   = [r|{ "type": "string", "minLength": 2, "maxLength": 3 }|]
        schema = Schema
          { schemaType             = Just (StringType stringType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
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
        { schemaType             = Just (StringType stringType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
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
          { schemaType             = Just (StringType stringType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        stringType = String
          { stringMinLength = Nothing
          , stringMaxLength = Nothing
          , stringPattern   = Nothing
          , stringFormat    = Just Byte
          }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
