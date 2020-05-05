{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import           Prelude                        ( print )
import           RIO
import           RIO.Directory                  ( createDirectoryIfMissing )
import           RIO.FilePath                   ( (</>)
                                                , addExtension
                                                )
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as B
import           Discovery.RestDescription

type BasePath = Text

gen :: BasePath -> RestDescription -> IO ()
gen path desc = do
  service <- T.toTitle <$> getName
  version <- T.toTitle <$> getVersion
  let serviceDir = T.unpack path </> T.unpack service </> T.unpack version
  createDirectoryIfMissing True serviceDir

  schemas <- getSchemas
  createSchemaFiles service version serviceDir schemas
 where
  getName    = get restDescriptionName "filed to get name."
  getVersion = get restDescriptionVersion "filed to get version."
  getSchemas = get restDescriptionSchemas "failed to get schemas."
  get f s = maybe (error s) pure (f desc)

createSchemaFiles :: Text -> Text -> FilePath -> RestDescriptionSchemas -> IO ()
createSchemaFiles service version serviceDir schemas =
  forM_ (Map.toList schemas) $ \(name, schema) -> do
    let dir = serviceDir </> "Schemas"
    createDirectoryIfMissing True dir
    let path = addExtension (dir </> T.unpack name) "hs"
    print path
    B.writeFile path (T.encodeUtf8 (createSchema service version name schema))

createSchema :: Text -> Text -> Text -> RestDescriptionParameter -> Text
createSchema service version name _ = T.unlines [moduleDef]
 where
  moduleDef  = T.intercalate " " ["module", moduleName, "where"]
  moduleName = T.intercalate "." [service, version, "Schemas", name]
