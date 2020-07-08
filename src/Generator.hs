module Generator
  ( gen
  , module Generator.Types
  , module Generator.Util
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified Discovery.RestDescription     as Desc
import           Generator.Types
import           Generator.Util
import qualified Generator.Project             as Proj
import qualified Generator.Schema              as Schema
import qualified Generator.Resource            as Resource

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
