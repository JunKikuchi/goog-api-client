module CodeGen where

import           RIO
import qualified RIO.Directory                 as Dir
import           Discovery.RestDescription      ( RestDescription )
import           CodeGen.Types
import qualified CodeGen.Project               as Proj

gen :: Dist -> RestDescription -> IO ()
gen dist desc = do
  Dir.createDirectoryIfMissing True dist
  Dir.withCurrentDirectory dist $ do
    projName <- Proj.projectName desc
    let projDir = Proj.projectDir projName
    Proj.clean projDir
    Proj.gen projName
