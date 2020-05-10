{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Project where

import           RIO
import qualified RIO.Directory                 as Dir
import qualified RIO.Text                      as T
import qualified RIO.Process                   as Proc
import           Discovery.RestDescription
import           CodeGen.Types
import           CodeGen.Utils                  ( get )

projectName :: RestDescription -> IO ProjectName
projectName desc =
  ("goog-api-client-" <>) <$> get restDescriptionName "name" desc

clean :: ProjectName -> IO ()
clean name = do
  t <- Dir.doesPathExist dir
  when t $ Dir.removeDirectoryRecursive dir
  where dir = T.unpack name

gen :: ProjectName -> RestDescription -> IO ()
gen name _desc = do
  let cmd = T.unpack ("stack new " <> name <> " goog-api-client.hsfiles")
  Proc.withProcessWait (fromString cmd) (const $ pure ())
