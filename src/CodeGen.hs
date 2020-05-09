module CodeGen where

import           RIO
import qualified CodeGen.Schema
import           Discovery.RestDescription

gen :: CodeGen.Schema.DestDir -> RestDescription -> IO ()
gen = CodeGen.Schema.gen
