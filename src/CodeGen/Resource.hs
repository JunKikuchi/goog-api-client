{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Resource where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           CodeGen.Types           hiding ( Schema )
import           CodeGen.Util

resourceName :: Text
resourceName = "Resource"

resourceDir :: SchemaDir
resourceDir = T.unpack resourceName

gen :: ServiceName -> ServiceVersion -> RestDescriptionResources -> IO ()
gen svcName svcVer resources = withDir resourceDir $ do
  dir <- Dir.getCurrentDirectory
  print dir
  forM_ (Map.toList resources) (uncurry $ createFile svcName svcVer)

createFile
  :: ServiceName
  -> ServiceVersion
  -> ResourceName
  -> RestDescriptionResource
  -> IO ()
createFile svcName svcVer resName _resource = do
  print moduleName
 where
  moduleName =
    T.intercalate "." [svcName, svcVer, resourceName, toCamelName resName]
