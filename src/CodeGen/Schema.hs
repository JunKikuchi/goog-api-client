{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           Discovery.RestDescription.Schema
import           CodeGen.Types
import           CodeGen.Utils                  ( get
                                                , withDir
                                                )

schemaDir :: SchemaDir
schemaDir = T.unpack "Schema"

gen :: ServiceName -> ServiceVersion -> RestDescriptionSchemas -> IO ()
gen name ver schemas = withDir schemaDir $ do
  dir <- Dir.getCurrentDirectory
  print dir

  mapM_ (genSchema name ver) schemas

genSchema :: ServiceName -> ServiceVersion -> Schema -> IO ()
genSchema name ver desc = do
  scmId <- get schemaId "schemaId" desc
  print [name, ver, scmId]
