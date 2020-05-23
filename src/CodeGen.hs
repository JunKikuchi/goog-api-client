{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

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

gen :: Dist -> RestDescription -> IO ()
gen dist desc = withDir dist $ do
  projName <- Proj.projectName desc
  let projDir = Proj.projectDir projName
  Proj.clean projDir
  Proj.gen projName
  withDir projDir $ withDir Proj.srcDir $ do
    svcName <- toTitle <$> get Desc.restDescriptionName "name" desc
    svcVer  <- toTitle <$> get Desc.restDescriptionVersion "version" desc
    let svcDir = Proj.serviceDir svcName svcVer
    withDir svcDir $ case Desc.restDescriptionSchemas desc of
      (Just schemas) -> Schema.gen svcName svcVer schemas
      _              -> pure ()
