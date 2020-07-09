{-# LANGUAGE OverloadedStrings #-}
module Generator.Resource.Data
  ( createData
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( runWriterT
                                                , tell
                                                )
import           Discovery.RestDescription
import           Generator.Types
import           Generator.Util
import           Generator.Schema.ImportInfo    ( createImport )
import           Generator.Schema.File          ( schemaName )
import           Generator.Schema.Types  hiding ( Schema )
import           Path

type ApiName = Text

defaultExtentions :: [Text]
defaultExtentions =
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  ]

defaultImports :: [Text]
defaultImports = ["import Servant.API"]

createData
  :: ModuleName
  -> ServiceName
  -> ServiceVersion
  -> RestDescriptionParameters
  -> RestDescriptionMethods
  -> IO Text
createData moduleName svcName svcVer commonParams methods = do
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
  pure content


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
createMethod commonParams _name method = do
  methodId   <- get restDescriptionMethodId "method id" method
  path       <- get restDescriptionMethodPath "method path" method
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  let apiName  = toCamelName methodId
      pathName = unTitle $ apiName <> "Path"
  createPath    <- createPathFunction params pathName path
  captures      <- createCapture pathName
  queries       <- createQueryParam params
  commonQueries <- createQueryParam commonParams
  request       <- createRequestBody $ restDescriptionMethodRequest method
  response      <- createResponseBody $ restDescriptionMethodResponse method
  let reqBody = createReqBody request
      verb    = createVerb httpMethod response
      desc    = descContent 0 $ restDescriptionMethodDescription method
      apiPath =
        T.intercalate "\n  :>\n"
          $  captures
          <> queries
          <> commonQueries
          <> reqBody
          <> verb
      apiType = "type " <> apiName <> "\n  =\n" <> apiPath
  pure (apiName, desc <> apiType <> "\n\n" <> createPath)
  where params = fromMaybe Map.empty $ restDescriptionMethodParameters method

createPathFunction
  :: MonadThrow m
  => RestDescriptionParameters
  -> Text
  -> Text
  -> GenImport m Text
createPathFunction params pathName path = do
  tell (Set.singleton ImportPrelude)
  paths    <- either throwM pure $ Path.parse path
  pathArgs <- createPathParams paths
  let argNames = foldr f [] . join . pathSegments $ paths
  types <- createPathTypes pathParams argNames
  let functionType = pathName <> " :: " <> T.intercalate
        " "
        (L.intersperse "->" (types <> ["[RIO.Text]"]))
      functionBody =
        pathName
          <> " "
          <> T.intercalate " " argNames
          <> " = join ["
          <> T.intercalate ", " pathArgs
          <> "]"
  pure $ functionType <> "\n" <> functionBody <> "\n"
 where
  pathParams = Map.filter filterPath params
  filterPath schema = schemaLocation schema == Just "path"
  f (Expression _ name) acc = name : acc
  f _                   acc = acc

createPathTypes
  :: MonadThrow m => RestDescriptionParameters -> [Text] -> GenImport m [Text]
createPathTypes params argNames = sequence $ argTypes <$> argNames
 where
  argTypes name = maybe
    (throwM . GeneratorException $ "could not find param '" <> name <> "'")
    paramType
    (Map.lookup name params)

createPathParams :: MonadThrow m => Path -> GenImport m [Text]
createPathParams path = sequence $ segment <$> pathSegments path
 where
  segment = fmap (T.intercalate " <> ") . traverse template
  template :: MonadThrow m => Template -> GenImport m Text
  template (Literal a                   ) = pure $ "[\"" <> a <> "\"]"
  template (Expression Nothing         a) = pure $ "[" <> a <> "]"
  template (Expression (Just Reserved) a) = do
    tell (Set.singleton ImportText)
    pure $ "T.split (== '/') " <> a
  template _ = undefined

createCapture :: MonadThrow m => Text -> GenImport m [Text]
createCapture name = tell (Set.singleton ImportPrelude)
  >> pure ["  CaptureAll \"" <> name <> "\" RIO.Text"]

createQueryParam
  :: MonadThrow m => RestDescriptionParameters -> GenImport m [Text]
createQueryParam =
  traverse (uncurry createQueryParamElement) . filter filterQuery . Map.toList
  where filterQuery (_, schema) = schemaLocation schema == Just "query"

createQueryParamElement :: MonadThrow m => Text -> Schema -> GenImport m Text
createQueryParamElement name schema = do
  pt <- paramType schema
  pure $ "  " <> query <> " \"" <> name <> "\" " <> pt
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
      .  GeneratorException
      $  "not implemented schemaType '"
      <> T.pack (show st)
      <> "'"
  _ ->
    throwM
      .  GeneratorException
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
  f ImportText    = ["import qualified RIO.Text as T"]
  f (Import ref)  = [createImport "" svcName svcVer schemaName ref ref]
  f _             = undefined
