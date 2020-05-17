{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.JSON.Schema.Array where

import           RIO
import           RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.RawString.QQ              ( r )
import           Data.Aeson                     ( decode )
import           JSON.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_JSON_Schema_Array :: Spec
spec_Test_JSON_Schema_Array = describe "array" $ do
  describe "List validation" $ do
    let
      json   = [r|{ "type": "array", "items": { "type": "number" } }|]
      schema = Schema
        { schemaType             = Just (ArrayType arrayType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
      arrayType = Array
        { arrayItems           = Just
          (ArrayItemsItem
            (Schema
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
          )
        , arrayContains        = Nothing
        , arrayAdditionalItems = Nothing
        , arrayMinItems        = Nothing
        , arrayMaxItems        = Nothing
        , arrayUniqueItems     = Nothing
        }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Tuple validation" $ do
    let
      json
        = [r|{ "type": "array", "items": [{ "type": "number" }, { "type": "string" }, { "type": "string", "enum": ["Street", "Avenue", "Boulevard"] }, { "type": "string", "enum": ["NW", "NE", "SW", "SE"] }] }|]
      schema = Schema
        { schemaType             = Just (ArrayType arrayType)
        , schemaTitle            = Nothing
        , schemaDescription      = Nothing
        , schemaExamples         = Nothing
        , schemaComment          = Nothing
        , schemaEnum             = Nothing
        , schemaEnumDescriptions = Nothing
        , schemaConst            = Nothing
        }
      arrayType = Array
        { arrayItems           = Just
          (ArrayItemsTuple
            [ Schema
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
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              (Just ["Street", "Avenue", "Boulevard"])
              Nothing
              Nothing
            , Schema
              (Just (StringType (String Nothing Nothing Nothing Nothing)))
              Nothing
              Nothing
              Nothing
              Nothing
              (Just ["NW", "NE", "SW", "SE"])
              Nothing
              Nothing
            ]
          )
        , arrayContains        = Nothing
        , arrayAdditionalItems = Nothing
        , arrayMinItems        = Nothing
        , arrayMaxItems        = Nothing
        , arrayUniqueItems     = Nothing
        }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "additionalItems bool" $ do
      let
        json
          = [r|{ "type": "array", "items": [{ "type": "number" }, { "type": "string" }, { "type": "string", "enum": ["Street", "Avenue", "Boulevard"] }, { "type": "string", "enum": ["NW", "NE", "SW", "SE"] }], "additionalItems": false }|]
        schema = Schema
          { schemaType             = Just (ArrayType arrayType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        arrayType = Array
          { arrayItems           = Just
            (ArrayItemsTuple
              [ Schema
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
              , Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              , Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                (Just ["Street", "Avenue", "Boulevard"])
                Nothing
                Nothing
              , Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                (Just ["NW", "NE", "SW", "SE"])
                Nothing
                Nothing
              ]
            )
          , arrayContains        = Nothing
          , arrayAdditionalItems = Just (ArrayAdditionalItemsBool False)
          , arrayMinItems        = Nothing
          , arrayMaxItems        = Nothing
          , arrayUniqueItems     = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
    describe "additionalItems schema" $ do
      let
        json
          = [r|{ "type": "array", "items": [{ "type": "number" }, { "type": "string" }, { "type": "string", "enum": ["Street", "Avenue", "Boulevard"] }, { "type": "string", "enum": ["NW", "NE", "SW", "SE"] }], "additionalItems": { "type": "string" } }|]
        schema = Schema
          { schemaType             = Just (ArrayType arrayType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        arrayType = Array
          { arrayItems           = Just
            (ArrayItemsTuple
              [ Schema
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
              , Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              , Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                (Just ["Street", "Avenue", "Boulevard"])
                Nothing
                Nothing
              , Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                (Just ["NW", "NE", "SW", "SE"])
                Nothing
                Nothing
              ]
            )
          , arrayContains        = Nothing
          , arrayAdditionalItems = Just
            (ArrayAdditionalItemsSchema
              (Schema
                (Just (StringType (String Nothing Nothing Nothing Nothing)))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              )
            )
          , arrayMinItems        = Nothing
          , arrayMaxItems        = Nothing
          , arrayUniqueItems     = Nothing
          }
      it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Length" $ do
    let json   = [r|{ "type": "array", "minItems": 2, "maxItems": 3 }|]
        schema = Schema
          { schemaType             = Just (ArrayType arrayType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        arrayType = Array
          { arrayItems           = Nothing
          , arrayContains        = Nothing
          , arrayAdditionalItems = Nothing
          , arrayMinItems        = Just 2
          , arrayMaxItems        = Just 3
          , arrayUniqueItems     = Nothing
          }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
  describe "Uniqueness" $ do
    let json   = [r|{ "type": "array", "uniqueItems": true }|]
        schema = Schema
          { schemaType             = Just (ArrayType arrayType)
          , schemaTitle            = Nothing
          , schemaDescription      = Nothing
          , schemaExamples         = Nothing
          , schemaComment          = Nothing
          , schemaEnum             = Nothing
          , schemaEnumDescriptions = Nothing
          , schemaConst            = Nothing
          }
        arrayType = Array
          { arrayItems           = Nothing
          , arrayContains        = Nothing
          , arrayAdditionalItems = Nothing
          , arrayMinItems        = Nothing
          , arrayMaxItems        = Nothing
          , arrayUniqueItems     = Just True
          }
    it "Schema にエンコード" $ decode json `shouldBe` Just schema
