{-# LANGUAGE OverloadedStrings #-}
module Generator.Project
  ( gen
  )
where

import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Process                   as Proc
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           Generator.Types
import           Generator.Util

projectName :: MonadThrow m => RestDescription -> m ProjectName
projectName desc = do
  name <- get restDescriptionName "name" desc
  pure $ "goog-api-client-" <> T.intercalate "-" (T.split (== '_') name)

srcDir :: SrcDir
srcDir = T.unpack "src"

serviceDir :: ServiceName -> ServiceVersion -> ServiceDir
serviceDir name ver = T.unpack name </> T.unpack ver

gen :: RestDescription -> (ServiceName -> ServiceVersion -> IO a) -> IO a
gen desc action = do
  projName <- projectName desc
  let projDir = T.unpack projName
      cmd = T.unpack ("stack new " <> projName <> " goog-api-client.hsfiles")
  Proc.withProcessWait (fromString cmd) (const $ pure ())
  withDir projDir $ withDir srcDir $ do
    svcName <- toTitle <$> get restDescriptionName "name" desc
    svcVer  <- toTitle <$> get restDescriptionVersion "version" desc
    let svcDir = serviceDir svcName svcVer
    withDir svcDir $ action svcName svcVer
