{-# LANGUAGE OverloadedStrings #-}
module Generator.Resource.File
  ( gen
  )
where

import           Prelude                        ( print )

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT )
import           Discovery.RestDescription
import           Generator.Types
import           Generator.Util
import qualified Generator.Resource.Data       as Data
import           Generator.Schema.File          ( schemaName )
import qualified Generator.Schema.ImportInfo   as ImportInfo
import           Generator.Schema.Types  hiding ( Schema )

resourceName :: Text
resourceName = "Resource"

resourceDir :: SchemaDir
resourceDir = T.unpack resourceName

defaultExtentions :: [Text]
defaultExtentions =
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  ]

defaultImports :: [Text]
defaultImports = ["import Servant.API"]

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
    Just methods -> withDir dir $ forM_ (Map.elems methods) $ \method -> do
      ((apiName, body), imports) <- runWriterT
        $ Data.createData commonParams method
      let moduleName =
            T.intercalate "."
              $  [svcName, svcVer, resourceName]
              <> resNames
              <> [name, apiName]
          path = FP.addExtension (T.unpack apiName) "hs"
          importList =
            L.sort
              $  defaultImports
              <> ImportInfo.createImports svcName
                                          svcVer
                                          schemaName
                                          ""
                                          ImportInfo.empty
                                          imports
          content =
            flip T.snoc '\n'
              . unLines
              $ [ T.intercalate "\n" defaultExtentions
                , "module " <> moduleName <> " where"
                , T.intercalate "\n" importList
                , body
                ]
      print moduleName
      B.writeFile path (T.encodeUtf8 content)
    _ -> pure ()
  case restDescriptionResourceResources resource of
    Just resources -> withDir dir $ forM_
      (Map.toList resources)
      (uncurry $ createFile svcName svcVer commonParams (resNames <> [name]))
    _ -> pure ()
 where
  dir  = T.unpack name
  name = toCamelName resName
