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
    projectName <- Proj.projectName desc
    Proj.clean projectName
    Proj.gen projectName desc
