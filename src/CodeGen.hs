{-# LANGUAGE OverloadedStrings #-}
module CodeGen
  ( gen
  , module CodeGen.Types
  , module CodeGen.Util
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           Discovery.RestDescription      ( RestDescription )
import qualified Discovery.RestDescription     as Desc
import           CodeGen.Types
import           CodeGen.Util
import qualified CodeGen.Project               as Proj
import qualified CodeGen.Schema                as Schema
import qualified CodeGen.Resource              as Resource

gen :: Dist -> RestDescription -> IO ()
gen dist desc = withDir dist $ do
  projName <- Proj.projectName desc
  Proj.gen projName
  let projDir = Proj.projectDir projName
  withDir projDir $ withDir Proj.srcDir $ do
    svcName <- toTitle <$> get Desc.restDescriptionName "name" desc
    svcVer  <- toTitle <$> get Desc.restDescriptionVersion "version" desc
    let svcDir = Proj.serviceDir svcName svcVer
    withDir svcDir $ do
      case Desc.restDescriptionSchemas desc of
        (Just schemas) -> Schema.gen svcName svcVer schemas
        _              -> pure ()
      case Desc.restDescriptionResources desc of
        (Just resources) -> do
          let commonParams =
                fromMaybe Map.empty $ Desc.restDescriptionParameters desc
          Resource.gen svcName svcVer commonParams resources
        _ -> pure ()
