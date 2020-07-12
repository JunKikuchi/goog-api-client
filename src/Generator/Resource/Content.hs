{-# LANGUAGE OverloadedStrings #-}
module Generator.Resource.Content
  ( createContent
  )
where

import           RIO                     hiding ( Data )
import qualified RIO.Char                      as C
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
import           Generator.Resource.Types
import qualified Generator.Schema.Content      as C
import           Path

type ApiName = Text

{-
createApi :: [ApiName] -> Text
createApi apiNames = "type API\n  =    " <> api
  where api = T.intercalate "\n  :<|> " apiNames
-}

createContent
  :: MonadThrow m
  => ModuleName
  -> ApiName
  -> RestDescriptionParameters
  -> RestDescriptionMethod
  -> GenImport m Text
createContent moduleName apiName commonParams method = do
  ((apiType, paths), gens) <- runWriterT $ do
    apiType <- createApiType apiName pathName commonParams method
    paths   <- createPaths apiName pathName method
    pure (apiType, paths)
  (enums, imports) <- runWriterT $ createEnums moduleName gens
  tell imports
  let content =
        T.intercalate "\n\n" . filter (not . T.null) $ [apiType, enums, paths]
  pure content
  where pathName = unTitle apiName <> "Path"

createApiType
  :: MonadThrow m
  => ApiName
  -> Text
  -> RestDescriptionParameters
  -> RestDescriptionMethod
  -> GenData m Text
createApiType apiName pathName commonParams method = do
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  captures      <- createCapture pathName
  queries       <- createQueryParam apiName params
  commonQueries <- createQueryParam apiName commonParams
  reqBody       <- createRequestBody upload request
  verb          <- createResponseBody httpMethod response
  let apiPath =
        T.intercalate "\n  :>\n"
          $  captures
          <> queries
          <> commonQueries
          <> uploadTypeQuery
          <> reqBody
          <> verb
  pure $ desc <> "type " <> apiName <> "\n  =\n" <> apiPath
 where
  params          = fromMaybe Map.empty $ restDescriptionMethodParameters method
  upload = fromMaybe False $ restDescriptionMethodSupportsMediaUpload method
  request         = restDescriptionMethodRequest method
  response        = restDescriptionMethodResponse method
  uploadTypeQuery = [ "  QueryParam \"uploadType\" RIO.Text" | upload ]
  desc            = descContent 0 $ restDescriptionMethodDescription method

createPaths
  :: MonadThrow m => Text -> Text -> RestDescriptionMethod -> GenData m Text
createPaths apiName pathName method = do
  let cpath = createPath apiName params
  path                <- get restDescriptionMethodPath "method path" method
  createpath          <- cpath pathName (Just path) paramOrder
  createSimplePath    <- cpath simplePathName uploadSimplePath paramOrder
  createResumablePath <- cpath resumablePathName uploadResumablePath paramOrder
  pure
    $ T.intercalate "\n\n"
    . filter (not . T.null)
    $ [createpath, createSimplePath, createResumablePath]
 where
  pname             = unTitle apiName
  simplePathName    = pname <> "SimplePath"
  resumablePathName = pname <> "ResumablePath"
  params = fromMaybe Map.empty $ restDescriptionMethodParameters method
  paramOrder        = fromMaybe [] $ restDescriptionMethodParameterOrder method
  mediaUpload       = restDescriptionMethodMediaUpload method
  uploadProtocols   = mediaUpload >>= restDescriptionMethodMediaProtocols
  uploadSimplePath =
    uploadProtocols
      >>= restDescriptionMethodMediaProtocolsSimple
      >>= restDescriptionMethodMediaProtocolsSimplePath
  uploadResumablePath =
    uploadProtocols
      >>= restDescriptionMethodMediaProtocolsResumable
      >>= restDescriptionMethodMediaProtocolsResumablePath

createEnums :: MonadThrow m => ModuleName -> [Data] -> GenImport m Text
createEnums moduleName = fmap unLines . foldr f (pure mempty)
 where
  f :: MonadThrow m => Data -> GenImport m [Text] -> GenImport m [Text]
  f (DataEnum (name, enums)) acc = do
    let a     = C.createFieldEnumContent name enums
        aeson = C.createFieldEnumAesonContent moduleName name enums
    ((a <> "\n\n" <> aeson) :) <$> acc
  f (DataImport ref) acc = do
    tell $ Set.singleton ref
    acc

createPath
  :: MonadThrow m
  => ApiName
  -> RestDescriptionParameters
  -> Text
  -> Maybe Text
  -> [Text]
  -> GenData m Text
createPath _       _      _        Nothing     _          = pure ""
createPath apiName params pathName (Just path) paramOrder = do
  tell [DataImport ImportPrelude]
  paths <- either throwM pure $ Path.parse path
  let segments = pathSegments paths
      argNames = foldr f [] . join $ segments
  pathArgs <- createPathParams segments
  types    <- createPathTypes apiName pathParams argNames
  let functionType = desc <> pathName <> " :: " <> T.intercalate
        " "
        (L.intersperse "->" (types <> ["RIO.Text"]))
      functionBody =
        pathName
          <> " "
          <> T.intercalate " " argNames
          <> " = T.intercalate \"/\" $ join ["
          <> T.intercalate ", " pathArgs
          <> "]"
      content = T.intercalate "\n" [functionType, functionBody]
  pure content
 where
  pathParams = Map.filter filterPath params
  filterPath schema = schemaLocation schema == Just "path"
  f (Expression _ name) acc = name : acc
  f _                   acc = acc
  desc =
    descContent 0
      .  Just
      $  "Create CaptureAll path parameter for "
      <> apiName
      <> (if null paramOrder
           then ""
           else "\n\nArgs: " <> T.intercalate ", " paramOrder
         )

createPathTypes
  :: MonadThrow m
  => ApiName
  -> RestDescriptionParameters
  -> [Text]
  -> GenData m [Text]
createPathTypes apiName params argNames = sequence $ argTypes <$> argNames
 where
  argTypes name = maybe
    (throwM . GeneratorException $ "could not find param '" <> name <> "'")
    (paramType apiName name)
    (Map.lookup name params)

createPathParams :: MonadThrow m => [Segment] -> GenData m [Text]
createPathParams segments = sequence $ segment <$> segments
 where
  segment = fmap cat . traverse template
  cat [] = "[\"\"]"
  cat xs = T.intercalate " <> " xs
  template :: MonadThrow m => Template -> GenData m Text
  template (Literal a                   ) = pure $ "[\"" <> a <> "\"]"
  template (Expression Nothing         a) = pure $ "[" <> a <> "]"
  template (Expression (Just Reserved) a) = do
    tell [DataImport ImportPrelude]
    pure $ "T.split (== '/') " <> a
  template _ = undefined

createCapture :: MonadThrow m => Text -> GenData m [Text]
createCapture name = do
  tell [DataImport ImportPrelude]
  pure ["  CaptureAll \"" <> name <> "\" RIO.Text"]

createQueryParam
  :: MonadThrow m => ApiName -> RestDescriptionParameters -> GenData m [Text]
createQueryParam apiName =
  traverse (uncurry $ createQueryParamElement apiName)
    . filter filterQuery
    . Map.toList
  where filterQuery (_, schema) = schemaLocation schema == Just "query"

createQueryParamElement
  :: MonadThrow m => ApiName -> Text -> Schema -> GenData m Text
createQueryParamElement apiName name schema = do
  pt <- paramType apiName name schema
  pure $ "  " <> query <> " \"" <> name <> "\" " <> pt
 where
  query | schemaRepeated schema == Just True -- TODO: required 対応
                                             = "QueryParams"
        | schemaRequired schema == Just True = "QueryParam' '[Required, Strict]"
        | otherwise                          = "QueryParam"

paramType :: MonadThrow m => ApiName -> Text -> Schema -> GenData m Text
paramType apiName name schema = case schemaType schema of
  Just (StringType _) ->
    stringParamType (apiName <> toTitle (T.filter C.isAlphaNum name)) schema
  Just (IntegerType _) -> tell [DataImport ImportPrelude] >> pure "RIO.Int"
  Just (NumberType  _) -> tell [DataImport ImportPrelude] >> pure "RIO.Float"
  Just BooleanType     -> tell [DataImport ImportPrelude] >> pure "RIO.Bool"
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

stringParamType :: MonadThrow m => Text -> Schema -> GenData m Text
stringParamType name schema = case schemaEnum schema of
  (Just descEnum) -> do
    let descs = fromMaybe (L.repeat "") $ schemaEnumDescriptions schema
        enums = zip descEnum descs
    tell
      [ DataEnum (name, enums)
      , DataImport ImportPrelude
      , DataImport ImportGenerics
      , DataImport ImportJSON
      , DataImport ImportMap
      ]
    pure name
  Nothing -> tell [DataImport ImportPrelude] >> pure "RIO.Text"

createRequestBody
  :: MonadThrow m
  => Bool
  -> Maybe RestDescriptionMethodRequest
  -> GenData m [Text]
createRequestBody upload req =
  case maybe Nothing restDescriptionMethodRequestRef req of
    (Just ref) -> do
      tell [DataImport $ Import ref]
      pure $ createReqBody upload ref
    _ -> pure []

createReqBody :: Bool -> Text -> [Text]
createReqBody False ref = ["  ReqBody '[JSON] " <> ref <> "." <> ref]
createReqBody True ref =
  ["  ReqBody '[JSON] (Maybe " <> ref <> "." <> ref <> ")"]

createResponseBody
  :: MonadThrow m
  => Text
  -> Maybe RestDescriptionMethodResponse
  -> GenData m [Text]
createResponseBody method resp =
  case maybe Nothing restDescriptionMethodResponseRef resp of
    (Just ref) -> do
      tell [DataImport $ Import ref]
      pure [m <> " '[JSON] " <> ref <> "." <> ref]
    _ -> pure [m <> "NoContent '[JSON] NoContent"]
  where m = "  " <> T.toTitle method
