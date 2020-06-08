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

gen :: ServiceName -> ServiceVersion -> RestDescriptionResources -> IO ()
gen svcName svcVer resources = withDir resourceDir $ do
  Dir.getCurrentDirectory >>= print
  forM_ (Map.toList resources) (uncurry $ createFile svcName svcVer [])

createFile
  :: ServiceName
  -> ServiceVersion
  -> [ResourceName]
  -> ResourceName
  -> RestDescriptionResource
  -> IO ()
createFile svcName svcVer resNames resName resource = do
  case restDescriptionResourceMethods resource of
    Just methods -> do
      print moduleName
      (cs, imports) <- runWriterT $ createMethods methods
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
      forM_ (Map.toList resources)
            (uncurry $ createFile svcName svcVer (resNames <> [name]))
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
  => Map MethodName RestDescriptionMethod
  -> GenImport m [(ApiName, Text)]
createMethods methods = forM (Map.toList methods) (uncurry createMethod)

createMethod
  :: MonadThrow m
  => MethodName
  -> RestDescriptionMethod
  -> GenImport m (ApiName, Text)
createMethod _name method = do
  methodId   <- get restDescriptionMethodId "method id" method
  path       <- get restDescriptionMethodPath "method path" method
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  captures   <- createCapture params path
  queries    <- createQueryParam params
  request    <- createRequestBody $ restDescriptionMethodRequest method
  response   <- createResponseBody $ restDescriptionMethodResponse method
  let
    reqBody = createReqBody request
    verb    = createVerb httpMethod response
    desc    = descContent 0 $ restDescriptionMethodDescription method
    apiName = toCamelName methodId
    apiPath = T.intercalate "\n  :>\n" $ captures <> queries <> reqBody <> verb
    apiType = "type " <> apiName <> "\n  =\n" <> apiPath
  pure (apiName, desc <> apiType <> "\n")
  where params = fromMaybe Map.empty $ restDescriptionMethodParameters method

createCapture
  :: MonadThrow m => RestDescriptionParameters -> Text -> GenImport m [Text]
createCapture params path = forM (T.split (== '/') path)
  $ createCaptureElement pathParams
 where
  pathParams = Map.filter filterPath params
  filterPath schema = schemaLocation schema == Just "path"

createCaptureElement
  :: MonadThrow m => RestDescriptionParameters -> Text -> GenImport m Text
createCaptureElement params s
  | T.take 1 s == "{" = do
    ct <- captureType
    pure $ desc <> "  Capture \"" <> name <> "\" " <> ct
  | otherwise = pure $ desc <> "  \"" <> s <> "\""
 where
  desc        = descContent 2 $ schema >>= schemaDescription
  schema      = Map.lookup name params
  name        = T.dropEnd 1 . T.drop 1 $ s
  captureType = do
    pt <- maybe (throwM . GetException $ "faild to lookup '" <> name <> "'")
                paramType
                schema
    pure $ required <> pt
  required = maybe "Maybe " (const "") $ schema >>= schemaRequired

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
