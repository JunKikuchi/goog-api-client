{-# LANGUAGE OverloadedStrings #-}
module Test.CodeGen.Resource where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           RIO.Writer                     ( runWriterT )
import           Test.Tasty
import           Test.Tasty.Hspec
import           CodeGen.Types
import           CodeGen.Resource
import           Discovery.RestDescription.Schema

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

emptySchema :: Discovery.RestDescription.Schema.Schema
emptySchema = Schema
  { schemaId               = Nothing
  , schemaType             = Nothing
  , schemaRef              = Nothing
  , schemaDescription      = Nothing
  , schemaDefault          = Nothing
  , schemaRequired         = Nothing
  , schemaEnum             = Nothing
  , schemaEnumDescriptions = Nothing
  , schemaRepeated         = Nothing
  , schemaLocation         = Nothing
  , schemaAnnotations      = Nothing
  }

emptyString :: Discovery.RestDescription.Schema.String
emptyString = String {stringPattern = Nothing, stringFormat = Nothing}


pathSchema :: Discovery.RestDescription.Schema.Schema
pathSchema = emptySchema { schemaLocation = Just "path"
                         , schemaType     = Just (StringType emptyString)
                         }

spec_Test_CodeGen_Resource :: Spec
spec_Test_CodeGen_Resource = describe "createCapture" $ do
  it "foo"
    $ let actual   = runWriterT (createCapture Map.empty "foo")
          expected = Just (["  \"foo\""], Set.empty)
      in  actual `shouldBe` expected
  it "foo/bar"
    $ let actual   = runWriterT (createCapture Map.empty "foo/bar")
          expected = Just (["  \"foo\"", "  \"bar\""], Set.empty)
      in  actual `shouldBe` expected
  it "{bar}"
    $ let
        actual = runWriterT
          (createCapture (Map.fromList [("bar", pathSchema)]) "{bar}")
        expected = Just
          (["  Capture \"bar\" Maybe RIO.Text"], Set.fromList [ImportPrelude])
      in
        actual `shouldBe` expected
