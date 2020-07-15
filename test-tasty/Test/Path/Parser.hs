{-# LANGUAGE OverloadedStrings #-}
module Test.Path.Parser where

import           RIO
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.Megaparsec
import           Path.Parser
import           Path.Types

{-# ANN module ("HLint: ignore Use camelCase" :: RIO.String) #-}

spec_Test_Path_Parser :: Spec
spec_Test_Path_Parser = describe "Parser" $ describe "path" $ do
  it "空文字" $ parse path "" "" `shouldBe` Right (Path [[]])
  it "///" $ parse path "" "///" `shouldBe` Right (Path [[], [], [], []])
  it "foo" $ parse path "" "foo" `shouldBe` Right (Path [[Literal "foo"]])
  it "foo/bar" $ parse path "" "foo/bar" `shouldBe` Right
    (Path [[Literal "foo"], [Literal "bar"]])
  it "{foo}" $ parse path "" "{foo}" `shouldBe` Right
    (Path [[Expression Nothing Nothing "foo"]])
  it "{foo:123}" $ parse path "" "{foo:123}" `shouldBe` Right
    (Path [[Expression Nothing (Just (Prefix 123)) "foo"]])
  it "{foo*}" $ parse path "" "{foo*}" `shouldBe` Right
    (Path [[Expression Nothing (Just Explode) "foo"]])
  it "{+foo}" $ parse path "" "{+foo}" `shouldBe` Right
    (Path [[Expression (Just Reserved) Nothing "foo"]])
  it "{#foo}" $ parse path "" "{#foo}" `shouldBe` Right
    (Path [[Expression (Just Fragment) Nothing "foo"]])
  it "{foo}/{+bar}" $ parse path "" "{foo}/{+bar}" `shouldBe` Right
    (Path
      [ [Expression Nothing Nothing "foo"]
      , [Expression (Just Reserved) Nothing "bar"]
      ]
    )
  it "foo{bar}" $ parse path "" "foo{bar}" `shouldBe` Right
    (Path [[Literal "foo", Expression Nothing Nothing "bar"]])
  it "foo{bar}:buzz" $ parse path "" "foo{bar}:buzz" `shouldBe` Right
    (Path [[Literal "foo", Expression Nothing Nothing "bar", Literal ":buzz"]])
  it "a/foo{bar}:buzz" $ parse path "" "a/foo{bar}:buzz" `shouldBe` Right
    (Path
      [ [Literal "a"]
      , [Literal "foo", Expression Nothing Nothing "bar", Literal ":buzz"]
      ]
    )
  it "a/foo{/bar}" $ parse path "" "a/foo{/bar}" `shouldBe` Right
    (Path
      [ [Literal "a"]
      , [Literal "foo", Expression (Just PathSegment) Nothing "bar"]
      ]
    )
  it "a/foo{/bar*}" $ parse path "" "a/foo{/bar*}" `shouldBe` Right
    (Path
      [ [Literal "a"]
      , [Literal "foo", Expression (Just PathSegment) (Just Explode) "bar"]
      ]
    )
