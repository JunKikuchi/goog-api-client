{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Project where

import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Process                   as Proc
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           CodeGen.Types
import           CodeGen.Util                   ( get )

projectName :: RestDescription -> IO ProjectName
projectName desc = do
  name <- get restDescriptionName "name" desc
  pure $ "goog-api-client-" <> T.intercalate "-" (T.split (== '_') name)

projectDir :: ProjectName -> ProjectDir
projectDir = T.unpack

srcDir :: SrcDir
srcDir = T.unpack "src"

serviceDir :: ServiceName -> ServiceVersion -> ServiceDir
serviceDir name ver = T.unpack name </> T.unpack ver

gen :: ProjectName -> IO ()
gen name = Proc.withProcessWait (fromString cmd) (const $ pure ())
  where cmd = T.unpack ("stack new " <> name <> " goog-api-client.hsfiles")
