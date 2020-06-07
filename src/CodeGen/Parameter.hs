{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Parameter
  ( gen
  , createParams
  )
where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO                     hiding ( Data )
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT )
import           Discovery.RestDescription
import           CodeGen.Data
import           CodeGen.Types           hiding ( Schema )
import           CodeGen.Util

parameterName :: Text
parameterName = "Parameter"

gen :: ServiceName -> ServiceVersion -> RestDescriptionParameters -> IO ()
gen svcName svcVer params = do
  dir <- Dir.getCurrentDirectory
  print dir
  print moduleName
  (contents, imports) <- createParams moduleName params
  let path    = FP.addExtension (T.unpack parameterName) "hs"
      content = T.intercalate
        "\n"
        [ "{-# LANGUAGE DeriveGeneric #-}"
        , "module " <> moduleName <> " where"
        , ""
        , imports
        , ""
        , contents
        , ""
        ]
  B.writeFile path (T.encodeUtf8 content)
  where moduleName = T.intercalate "." [svcName, svcVer, parameterName]

createParams
  :: MonadThrow m => ModuleName -> RestDescriptionParameters -> m (Text, Text)
createParams moduleName params = do
  (contents, imports) <- runWriterT $ Map.foldrWithKey cons (pure []) params
  pure
    ( createContent contents
    , createImports $ foldr selectImport Set.empty imports
    )
 where
  cons
    :: MonadThrow m
    => RecordName
    -> Schema
    -> GenData m [Text]
    -> GenData m [Text]
  cons name schema contents = do
    record <- createData moduleName (toCamelName name) schema
    (record :) <$> contents
  selectImport (DataImport imprt) = Set.insert imprt
  selectImport _                  = id

createContent :: [Text] -> Text
createContent = T.intercalate "\n\n"

createImports :: Set Import -> Text
createImports = T.intercalate "\n" . L.sort . join . fmap f . Set.toList
 where
  f ImportPrelude = ["import RIO"]
  f ImportEnum =
    ["import qualified Data.Aeson as Aeson", "import qualified RIO.Map as Map"]
  f ImportGenerics = ["import GHC.Generics()"]
  f _              = undefined
