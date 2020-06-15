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
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT
                                                , tell
                                                )
import           Discovery.RestDescription
import           CodeGen.Schema                 ( createImport )
import           CodeGen.Types           hiding ( Schema )
import           CodeGen.Util

type ApiName = Text

resourceName :: Text
resourceName = "Resource"

resourceDir :: SchemaDir
resourceDir = T.unpack resourceName

defaultExtentions :: [Text]
defaultExtentions =
  ["{-# LANGUAGE DataKinds #-}", "{-# LANGUAGE TypeOperators #-}"]

defaultImports :: [Text]
defaultImports = ["import Servant.API"]

gen
  :: ServiceName
  -> ServiceVersion
  -> RestDescriptionParameters
  -> RestDescriptionResources
  -> IO ()
gen svcName svcVer commonParams resources = withDir resourceDir $ do
  Dir.getCurrentDirectory >>= print
  forM_ (Map.toList resources)
        (uncurry $ createFile svcName svcVer commonParams [])

createFile
  :: ServiceName
  -> ServiceVersion
  -> RestDescriptionParameters
  -> [ResourceName]
  -> ResourceName
  -> RestDescriptionResource
  -> IO ()
createFile svcName svcVer commonParams resNames resName resource = do
  case restDescriptionResourceMethods resource of
    Just methods -> do
      print moduleName
      (cs, imports) <- runWriterT $ createMethods commonParams methods
      let (apiNames, contents) = L.unzip cs
          api                  = createApi apiNames
          content =
            T.intercalate "\n"
              $  defaultExtentions
              <> [ "module " <> moduleName <> " where"
                 , ""
                 , createImports svcName svcVer imports
                 , ""
                 , api
                 , ""
                 ]
              <> contents
      B.writeFile path (T.encodeUtf8 content)
    _ -> pure ()
  case restDescriptionResourceResources resource of
    Just resources -> withDir (T.unpack name) $ do
      Dir.getCurrentDirectory >>= print
      forM_
        (Map.toList resources)
        (uncurry $ createFile svcName svcVer commonParams (resNames <> [name]))
    _ -> pure ()
 where
  moduleName =
    T.intercalate "." $ [svcName, svcVer, resourceName] <> resNames <> [name]
  name = toCamelName resName
  path = FP.addExtension (T.unpack name) "hs"

createApi :: [ApiName] -> Text
createApi apiNames = "type API\n  =    " <> api
  where api = T.intercalate "\n  :<|> " apiNames

createMethods
  :: MonadThrow m
  => RestDescriptionParameters
  -> Map MethodName RestDescriptionMethod
  -> GenImport m [(ApiName, Text)]
createMethods commonParams methods =
  forM (Map.toList methods) (uncurry $ createMethod commonParams)

createMethod
  :: MonadThrow m
  => RestDescriptionParameters
  -> MethodName
  -> RestDescriptionMethod
  -> GenImport m (ApiName, Text)
createMethod commonParams name method = do
  methodId      <- get restDescriptionMethodId "method id" method
  _path         <- get restDescriptionMethodPath "method path" method
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  captures      <- createCapture name
  queries       <- createQueryParam params
  commonQueries <- createQueryParam commonParams
  request       <- createRequestBody $ restDescriptionMethodRequest method
  response      <- createResponseBody $ restDescriptionMethodResponse method
  let reqBody = createReqBody request
      verb    = createVerb httpMethod response
      desc    = descContent 0 $ restDescriptionMethodDescription method
      apiName = toCamelName methodId
      apiPath =
        T.intercalate "\n  :>\n"
          $  captures
          <> queries
          <> commonQueries
          <> reqBody
          <> verb
      apiType = "type " <> apiName <> "\n  =\n" <> apiPath
  pure (apiName, desc <> apiType <> "\n")
  where params = fromMaybe Map.empty $ restDescriptionMethodParameters method

createCapture :: MonadThrow m => MethodName -> GenImport m [Text]
createCapture name = tell (Set.singleton ImportPrelude)
  >> pure ["  CaptureAll \"" <> path <> "\" " <> toCamelName path]
  where path = name <> "Path"

createQueryParam
  :: MonadThrow m => RestDescriptionParameters -> GenImport m [Text]
createQueryParam =
  sequence
    . fmap (uncurry createQueryParamElement)
    . filter filterQuery
    . Map.toList
  where filterQuery (_, schema) = schemaLocation schema == Just "query"

createQueryParamElement :: MonadThrow m => Text -> Schema -> GenImport m Text
createQueryParamElement name schema = do
  pt <- paramType schema
  pure
    $  descContent 2 (schemaDescription schema)
    <> "  "
    <> query
    <> " \""
    <> name
    <> "\" "
    <> pt
 where
  query | schemaRepeated schema == Just True -- TODO: required 対応
                                             = "QueryParams"
        | schemaRequired schema == Just True = "QueryParam' '[Required, Strict]"
        | otherwise                          = "QueryParam"

paramType :: MonadThrow m => Schema -> GenImport m Text
paramType schema = case schemaType schema of
  Just (StringType  _) -> tell (Set.singleton ImportPrelude) >> pure "RIO.Text"
  Just (IntegerType _) -> tell (Set.singleton ImportPrelude) >> pure "RIO.Int"
  Just (NumberType  _) -> tell (Set.singleton ImportPrelude) >> pure "RIO.Float"
  Just BooleanType     -> tell (Set.singleton ImportPrelude) >> pure "RIO.Bool"
  Just st ->
    throwM
      .  GetException
      $  "not implemented schemaType '"
      <> T.pack (show st)
      <> "'"
  _ ->
    throwM
      .  GetException
      $  "failed to get schemaType '"
      <> T.pack (show schema)
      <> "'"

createRequestBody
  :: MonadThrow m
  => Maybe RestDescriptionMethodRequest
  -> GenImport m (Maybe Text)
createRequestBody req =
  case maybe Nothing restDescriptionMethodRequestRef req of
    (Just ref) -> do
      tell (Set.singleton $ Import ref)
      pure . pure $ ref
    _ -> pure Nothing

createResponseBody
  :: MonadThrow m
  => Maybe RestDescriptionMethodResponse
  -> GenImport m (Maybe Text)
createResponseBody resp =
  case maybe Nothing restDescriptionMethodResponseRef resp of
    (Just ref) -> do
      tell (Set.singleton $ Import ref)
      pure . pure $ ref
    _ -> pure Nothing

createReqBody :: Maybe Text -> [Text]
createReqBody (Just ref) = ["  ReqBody '[JSON] " <> ref <> "." <> ref]
createReqBody _          = []

createVerb :: Text -> Maybe Text -> [Text]
createVerb method resp = case resp of
  (Just ref) -> [m <> " '[JSON] " <> ref <> "." <> ref]
  _          -> [m <> "NoContent '[JSON] NoContent"]
  where m = "  " <> T.toTitle method

createImports :: ServiceName -> ServiceVersion -> Set Import -> Text
createImports svcName svcVer =
  T.intercalate "\n" . L.sort . (<>) defaultImports . join . fmap f . Set.toList
 where
  f ImportPrelude = ["import RIO"]
  f (Import ref)  = [createImport svcName svcVer ref ref False]
  f _             = undefined
