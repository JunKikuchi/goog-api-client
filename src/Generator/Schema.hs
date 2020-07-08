module Generator.Schema
  ( gen
  )
where

import           RIO
import           Discovery.RestDescription
import           Generator.Types
import qualified Generator.ImportInfo          as ImportInfo
import qualified Generator.Schema.File         as File
import qualified Generator.Util                as Util

gen :: ServiceName -> ServiceVersion -> RestDescriptionSchemas -> IO ()
gen svcName svcVer schemas = Util.withDir File.schemaDir
  $ foldM_ (File.createFile svcName svcVer) ImportInfo.empty schemas
