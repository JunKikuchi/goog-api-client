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
import           CodeGen.Parameter              ( createParams )
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
  let params = restDescriptionMethodParameters method
  (_contents, _imports) <- maybe (pure ("", ""))
                                 (createParams moduleName)
                                 params
  let
    _request  = restDescriptionMethodRequest method
    _response = restDescriptionMethodResponse method
    desc
      = maybe
          ""
          (\s -> "{-|\n" <> (T.unlines . fmap ("  " <>) . T.lines $ s) <> "-}\n"
          )
        $ restDescriptionMethodDescription method
    apiName = toCamelName methodId
    apiPath =
      T.intercalate "\n  :> "
        $  createCapture moduleName path
        <> maybe [] (createQueryParam moduleName) params
        -- <> maybe [] (createRequestBody moduleName)  request
        -- <> maybe [] (createResponseBody moduleName) response
    apiType = "type " <> apiName <> "\n  =  " <> apiPath
  pure $ desc <> apiType

createCapture :: ModuleName -> Text -> [Text]
createCapture moduleName path =
  createCaptureElement moduleName <$> T.split (== '/') path

createCaptureElement :: ModuleName -> Text -> Text
createCaptureElement moduleName s
  | T.take 1 s == "{"
  = "Capture \"" <> name <> "\" " <> moduleName <> "." <> toCamelName name
  | otherwise
  = "\"" <> s <> "\""
  where name = T.dropEnd 1 . T.drop 1 $ s

createQueryParam :: ModuleName -> RestDescriptionParameters -> [Text]
createQueryParam moduleName =
  fmap (createQueryParamElement moduleName) . filter filterQuery . Map.toList
  where filterQuery (_, schema) = schemaLocation schema == Just "query"

createQueryParamElement :: ModuleName -> (Text, Schema) -> Text
createQueryParamElement moduleName (name, schema) =
  query <> " \"" <> name <> "\" " <> moduleName <> "." <> toCamelName name -- TODO: required 対応
 where
  query | schemaRepeated schema == Just True = "QueryParams"
        | otherwise                          = "QueryParam"

{-
createRequestBody :: ModuleName -> RestDescriptionMethodRequest -> [Text]
createRequestBody = undefined

createResponseBody :: ModuleName -> RestDescriptionMethodResponse -> [Text]
createResponseBody = undefined
-}
