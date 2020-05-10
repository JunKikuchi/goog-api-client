{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Schema where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           CodeGen.Types
import           CodeGen.Utils                  ( withDir )

schemaDir :: SchemaDir
schemaDir = T.unpack "Schema"

gen :: RestDescription -> IO ()
gen _ = withDir schemaDir $ do
  dir <- Dir.getCurrentDirectory
  print dir
