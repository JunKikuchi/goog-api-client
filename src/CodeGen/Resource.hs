{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Resource
  ( gen
  )
where

import           Prelude                        ( print )
import qualified RIO.Directory                 as Dir

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.FilePath                  as FP
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           Discovery.RestDescription
import           CodeGen.Types           hiding ( Schema )
import           CodeGen.Util

resourceName :: Text
resourceName = "Resource"

resourceDir :: SchemaDir
resourceDir = T.unpack resourceName

gen :: ServiceName -> ServiceVersion -> RestDescriptionResources -> IO ()
gen svcName svcVer resources = withDir resourceDir $ do
  dir <- Dir.getCurrentDirectory
  print dir
  forM_ (Map.toList resources) (uncurry $ createFile svcName svcVer)

createFile
  :: ServiceName
  -> ServiceVersion
  -> ResourceName
  -> RestDescriptionResource
  -> IO ()
createFile svcName svcVer resName resource = do
  print moduleName
  contents <- case restDescriptionResourceMethods resource of
    Just methods -> createMethods moduleName methods
    _            -> pure []
  case restDescriptionResourceResources resource of
    Just _resources -> pure () -- print resources
    _               -> pure ()
  let content =
        T.intercalate "\n" ["module " <> moduleName <> " where", ""]
          <> T.intercalate "\n\n" contents
  B.writeFile path (T.encodeUtf8 content)
 where
  moduleName = T.intercalate "." [svcName, svcVer, resourceName, name]
  name       = toCamelName resName
  path       = FP.addExtension (T.unpack name) "hs"

createMethods
  :: MonadThrow m
  => ModuleName
  -> Map MethodName RestDescriptionMethod
  -> m [Text]
createMethods moduleName methods =
  forM (Map.toList methods) (uncurry $ createMethod moduleName)

createMethod
  :: MonadThrow m => ModuleName -> MethodName -> RestDescriptionMethod -> m Text
createMethod moduleName _name method = do
  methodId    <- get restDescriptionMethodId "method id" method
  path        <- get restDescriptionMethodPath "method path" method
  _httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  let params    = fromMaybe Map.empty $ restDescriptionMethodParameters method
      _request  = restDescriptionMethodRequest method
      _response = restDescriptionMethodResponse method
      desc      = descContent 0 $ restDescriptionMethodDescription method
      apiName   = toCamelName methodId
      apiPath =
        T.intercalate "\n  :>\n"
          $  createCapture moduleName params path
          <> createQueryParam moduleName params
          -- <> maybe [] (createRequestBody moduleName)  request
          -- <> maybe [] (createResponseBody moduleName) response
      apiType = "type " <> apiName <> "\n  =\n" <> apiPath
  pure $ desc <> apiType

createCapture :: ModuleName -> RestDescriptionParameters -> Text -> [Text]
createCapture moduleName params path =
  createCaptureElement moduleName pathParams <$> T.split (== '/') path
 where
  pathParams = Map.filter filterPath params
  filterPath schema = schemaLocation schema == Just "path"

createCaptureElement :: ModuleName -> RestDescriptionParameters -> Text -> Text
createCaptureElement moduleName params s
  | T.take 1 s == "{"
  = desc
    <> "  Capture \""
    <> name
    <> "\" "
    <> moduleName
    <> "."
    <> toCamelName name
  | otherwise
  = desc <> "  \"" <> s <> "\""
 where
  desc = descContent 2 (Map.lookup name params >>= schemaDescription)
  name = T.dropEnd 1 . T.drop 1 $ s

createQueryParam :: ModuleName -> RestDescriptionParameters -> [Text]
createQueryParam moduleName =
  fmap (createQueryParamElement moduleName) . filter filterQuery . Map.toList
  where filterQuery (_, schema) = schemaLocation schema == Just "query"

createQueryParamElement :: ModuleName -> (Text, Schema) -> Text
createQueryParamElement moduleName (name, schema) =
  descContent 2 (schemaDescription schema)
    <> "  "
    <> query
    <> " \""
    <> name
    <> "\" "
    <> moduleName
    <> "."
    <> toCamelName name
 where
  query | schemaRepeated schema == Just True -- TODO: required 対応
                                             = "QueryParams"
        | schemaRequired schema == Just True = "QueryParam' '[Required, Strict]"
        | otherwise                          = "QueryParam"

{-
createRequestBody :: ModuleName -> RestDescriptionMethodRequest -> [Text]
createRequestBody = undefined

createResponseBody :: ModuleName -> RestDescriptionMethodResponse -> [Text]
createResponseBody = undefined
-}
