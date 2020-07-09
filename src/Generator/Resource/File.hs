{-# LANGUAGE OverloadedStrings #-}
module Generator.Resource.File
  ( gen
  )
where

import           Prelude                        ( print )

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           Generator.Types
import           Generator.Util
import qualified Generator.Resource.Data       as Data
import           Generator.Schema.Types  hiding ( Schema )

resourceName :: Text
resourceName = "Resource"

resourceDir :: SchemaDir
resourceDir = T.unpack resourceName

gen
  :: ServiceName
  -> ServiceVersion
  -> RestDescriptionParameters
  -> RestDescriptionResources
  -> IO ()
gen svcName svcVer commonParams resources = withDir resourceDir $ forM_
  (Map.toList resources)
  (uncurry $ createFile svcName svcVer commonParams [])

createFile
  :: ServiceName
  -> ServiceVersion
  -> RestDescriptionParameters
  -> [ResourceName]
  -> ResourceName
  -> RestDescriptionResource
  -> IO ()
createFile svcName svcVer commonParams resNames resName resource = do
  case restDescriptionResourceMethods resource of
    Just methods -> do
      print moduleName
      content <- Data.createData moduleName svcName svcVer commonParams methods
      B.writeFile path (T.encodeUtf8 content)
    _ -> pure ()
  case restDescriptionResourceResources resource of
    Just resources -> withDir (T.unpack name) $ forM_
      (Map.toList resources)
      (uncurry $ createFile svcName svcVer commonParams (resNames <> [name]))
    _ -> pure ()
 where
  moduleName =
    T.intercalate "." $ [svcName, svcVer, resourceName] <> resNames <> [name]
  name = toCamelName resName
  path = FP.addExtension (T.unpack name) "hs"
