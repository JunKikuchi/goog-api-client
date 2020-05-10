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

  let srcDir = Proj.srcDir projDir
  withDir srcDir $ Schema.gen desc
