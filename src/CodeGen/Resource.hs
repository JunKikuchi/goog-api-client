{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Resource
  ( gen
  )
where

import           Prelude                        ( print
                                                , putStrLn
                                                )
import qualified RIO.Directory                 as Dir

import           RIO
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
  case restDescriptionResourceMethods resource of
    Just methods -> createMethods moduleName methods
    _            -> pure ()
  case restDescriptionResourceResources resource of
    Just _resources -> pure () -- print resources
    _               -> pure ()
 where
  moduleName = T.intercalate "." [svcName, svcVer, resourceName, name]
  name       = toCamelName resName

createMethods :: ModuleName -> Map MethodName RestDescriptionMethod -> IO ()
createMethods moduleName methods =
  forM_ (Map.toList methods) (uncurry $ createMethod moduleName)

createMethod :: ModuleName -> MethodName -> RestDescriptionMethod -> IO ()
createMethod moduleName name method = do
  methodId   <- get restDescriptionMethodId "method id" method
  path       <- get restDescriptionMethodPath "method path" method
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  let params = restDescriptionMethodParameters method
  (contents, imports) <- maybe (pure ("", "")) (createParams moduleName) params
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
        $  createPaths moduleName path
        <> maybe [] (createQueryParam moduleName) params
        -- <> maybe [] (createRequestBody moduleName)  request
        -- <> maybe [] (createResponseBody moduleName) response
    apiType = "type " <> apiName <> "\n  =  " <> apiPath
  print name
  print httpMethod
  putStrLn $ T.unpack $ desc <> apiType
  putStrLn $ T.unpack imports
  putStrLn $ T.unpack contents

createPaths :: ModuleName -> Text -> [Text]
createPaths moduleName path =
  createPathElement moduleName <$> T.split (== '/') path

createPathElement :: ModuleName -> Text -> Text
createPathElement moduleName s
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
  queryParam <> " \"" <> name <> "\" " <> moduleName <> "." <> toCamelName name -- TODO: required 対応
 where
  queryParam | schemaRepeated schema == Just True = "QueryParams"
             | otherwise                          = "QueryParam"

{-
createRequestBody :: ModuleName -> RestDescriptionMethodRequest -> [Text]
createRequestBody = undefined

createResponseBody :: ModuleName -> RestDescriptionMethodResponse -> [Text]
createResponseBody = undefined
-}
