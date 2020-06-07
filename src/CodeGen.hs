{-# LANGUAGE OverloadedStrings #-}
module CodeGen
  ( gen
  )
where

import           RIO
import           Discovery.RestDescription      ( RestDescription )
import qualified Discovery.RestDescription     as Desc
import           CodeGen.Types
import           CodeGen.Util                   ( get
                                                , toTitle
                                                , withDir
                                                )
import qualified CodeGen.Project               as Proj
import qualified CodeGen.Schema                as Schema
import qualified CodeGen.Parameter             as Parameter
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
      case Desc.restDescriptionParameters desc of
        (Just params) -> Parameter.gen svcName svcVer params
        _             -> pure ()
      case Desc.restDescriptionResources desc of
        (Just resources) -> Resource.gen svcName svcVer resources
        _                -> pure ()
