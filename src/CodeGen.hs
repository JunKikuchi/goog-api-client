module CodeGen
  ( gen
  , module CodeGen.Types
  , module CodeGen.Util
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified Discovery.RestDescription     as Desc
import           CodeGen.Types
import           CodeGen.Util
import qualified CodeGen.Project               as Proj
import qualified CodeGen.Schema                as Schema
import qualified CodeGen.Resource              as Resource

gen :: DistDir -> Desc.RestDescription -> IO ()
gen dist desc = withDir dist $ Proj.gen desc $ \svcName svcVer -> do
  case Desc.restDescriptionSchemas desc of
    (Just schemas) -> Schema.gen svcName svcVer schemas
    _              -> pure ()
  case Desc.restDescriptionResources desc of
    (Just resources) -> do
      let commonParams =
            fromMaybe Map.empty $ Desc.restDescriptionParameters desc
      Resource.gen svcName svcVer commonParams resources
    _ -> pure ()
