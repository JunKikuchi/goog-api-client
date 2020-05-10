module CodeGen where

import           RIO
import           Discovery.RestDescription      ( RestDescription )
import           CodeGen.Types
import           CodeGen.Utils                  ( withDir )
import qualified CodeGen.Project               as Proj
import qualified CodeGen.Schema                as Schema

gen :: Dist -> RestDescription -> IO ()
gen dist desc = withDir dist $ do
  projName <- Proj.projectName desc
  let projDir = Proj.projectDir projName
  Proj.clean projDir
  Proj.gen projName

  withDir projDir $ withDir Proj.srcDir $ Schema.gen desc
