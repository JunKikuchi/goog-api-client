{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Project where

import           RIO
import qualified RIO.Directory                 as Dir
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Process                   as Proc
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           CodeGen.Types
import           CodeGen.Utils                  ( get )

projectName :: RestDescription -> IO ProjectName
projectName desc =
  ("goog-api-client-" <>) <$> get restDescriptionName "name" desc

projectDir :: ProjectName -> ProjectDir
projectDir = T.unpack


clean :: ProjectDir -> IO ()
clean dir = do
  t <- Dir.doesPathExist dir
  when t $ Dir.removeDirectoryRecursive dir

gen :: ProjectName -> IO ()
gen name = Proc.withProcessWait (fromString cmd) (const $ pure ())
  where cmd = T.unpack ("stack new " <> name <> " goog-api-client.hsfiles")
