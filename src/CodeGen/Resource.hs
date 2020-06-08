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
    Just methods -> createMethods methods
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
  :: MonadThrow m => Map MethodName RestDescriptionMethod -> m [Text]
createMethods methods = forM (Map.toList methods) (uncurry createMethod)

createMethod :: MonadThrow m => MethodName -> RestDescriptionMethod -> m Text
createMethod _name method = do
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
          $  createCapture params path
          <> createQueryParam params
          -- <> maybe [] (createRequestBody moduleName)  request
          -- <> maybe [] (createResponseBody moduleName) response
      apiType = "type " <> apiName <> "\n  =\n" <> apiPath
  pure $ desc <> apiType

createCapture :: RestDescriptionParameters -> Text -> [Text]
createCapture params path =
  createCaptureElement pathParams <$> T.split (== '/') path
 where
  pathParams = Map.filter filterPath params
  filterPath schema = schemaLocation schema == Just "path"

createCaptureElement :: RestDescriptionParameters -> Text -> Text
createCaptureElement params s
  | T.take 1 s == "{" = desc <> "  Capture \"" <> name <> "\" " <> captureType
  | otherwise         = desc <> "  \"" <> s <> "\""
 where
  desc        = descContent 2 $ schema >>= schemaDescription
  schema      = Map.lookup name params
  name        = T.dropEnd 1 . T.drop 1 $ s
  captureType = required <> maybe "" paramType schema
  required    = maybe "Maybe " (const "") $ schema >>= schemaRequired

createQueryParam :: RestDescriptionParameters -> [Text]
createQueryParam =
  fmap createQueryParamElement . filter filterQuery . Map.toList
  where filterQuery (_, schema) = schemaLocation schema == Just "query"

createQueryParamElement :: (Text, Schema) -> Text
createQueryParamElement (name, schema) =
  descContent 2 (schemaDescription schema)
    <> "  "
    <> query
    <> " \""
    <> name
    <> "\" "
    <> paramType schema
 where
  query | schemaRepeated schema == Just True -- TODO: required 対応
                                             = "QueryParams"
        | schemaRequired schema == Just True = "QueryParam' '[Required, Strict]"
        | otherwise                          = "QueryParam"

paramType :: Schema -> Text
paramType schema = case schemaType schema of
  Just (StringType  _) -> "RIO.Text"
  Just (IntegerType _) -> "RIO.Int"
  Just (NumberType  _) -> "RIO.Float"
  Just BooleanType     -> "RIO.Bool"
  _                    -> undefined

{-
createRequestBody :: ModuleName -> RestDescriptionMethodRequest -> [Text]
createRequestBody = undefined

createResponseBody :: ModuleName -> RestDescriptionMethodResponse -> [Text]
createResponseBody = undefined
-}
