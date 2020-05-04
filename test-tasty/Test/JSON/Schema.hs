{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema where

import           Prelude                        ( print )
import           RIO
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema :: Spec
spec_Test_JSON_Schema =
  describe "parse" $ describe "Type-specific keywords" $ do
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
    it "string" $ decode json `shouldBe` Just schema
