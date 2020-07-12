{-# LANGUAGE OverloadedStrings #-}
module Generator.Resource.Content
  ( createContent
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           RIO.Writer                     ( tell )
import           Discovery.RestDescription
import           Generator.Types
import           Generator.Util
import           Generator.Resource.Types
import           Path

type ApiName = Text

{-
createApi :: [ApiName] -> Text
createApi apiNames = "type API\n  =    " <> api
  where api = T.intercalate "\n  :<|> " apiNames
-}

createContent
  :: MonadThrow m
  => RestDescriptionParameters
  -> RestDescriptionMethod
  -> GenData m (ApiName, Text)
createContent commonParams method = do
  methodId   <- get restDescriptionMethodId "method id" method
  path       <- get restDescriptionMethodPath "method path" method
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  let apiName           = toCamelName methodId
      pathName          = unTitle $ apiName <> "Path"
      simplePathName    = unTitle $ apiName <> "SimplePath"
      resumablePathName = unTitle $ apiName <> "ResumablePath"
  createpath       <- createPath apiName params pathName (Just path) paramOrder
  createSimplePath <- createPath apiName
                                 params
                                 simplePathName
                                 uploadSimplePath
                                 paramOrder
  createResumablePath <- createPath apiName
                                    params
                                    resumablePathName
                                    uploadResumablePath
                                    paramOrder
  captures      <- createCapture pathName
  queries       <- createQueryParam apiName params
  commonQueries <- createQueryParam apiName commonParams
  request       <- createRequestBody $ restDescriptionMethodRequest method
  response      <- createResponseBody $ restDescriptionMethodResponse method
  let reqBody = createReqBody upload request
      verb    = createVerb httpMethod response
      desc    = descContent 0 $ restDescriptionMethodDescription method
      apiPath =
        T.intercalate "\n  :>\n"
          $  captures
          <> queries
          <> commonQueries
          <> uploadTypeQuery
          <> reqBody
          <> verb
      apiType = "type " <> apiName <> "\n  =\n" <> apiPath
      content =
        T.intercalate "\n\n"
          . filter (not . T.null)
          $ [desc <> apiType, createpath, createSimplePath, createResumablePath]
  pure (apiName, content)
 where
  params          = fromMaybe Map.empty $ restDescriptionMethodParameters method
  paramOrder      = fromMaybe [] $ restDescriptionMethodParameterOrder method
  upload = fromMaybe False $ restDescriptionMethodSupportsMediaUpload method
  uploadTypeQuery = [ "  QueryParam \"uploadType\" RIO.Text" | upload ]
  mediaUpload     = restDescriptionMethodMediaUpload method
  uploadProtocols = mediaUpload >>= restDescriptionMethodMediaProtocols
  uploadSimple = uploadProtocols >>= restDescriptionMethodMediaProtocolsSimple
  uploadSimplePath =
    uploadSimple >>= restDescriptionMethodMediaProtocolsSimplePath
  uploadResumable =
    uploadProtocols >>= restDescriptionMethodMediaProtocolsResumable
  uploadResumablePath =
    uploadResumable >>= restDescriptionMethodMediaProtocolsResumablePath

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
createCapture name = tell [DataImport ImportPrelude]
  >> pure ["  CaptureAll \"" <> name <> "\" RIO.Text"]

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
  Just (StringType  _) -> stringParamType apiName name schema
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

stringParamType :: MonadThrow m => ApiName -> Text -> Schema -> GenData m Text
stringParamType apiName name schema = case schemaEnum schema of
  (Just _descEnum) -> pure $ apiName <> toTitle name
  Nothing          -> tell [DataImport ImportPrelude] >> pure "RIO.Text"

createRequestBody
  :: MonadThrow m
  => Maybe RestDescriptionMethodRequest
  -> GenData m (Maybe Text)
createRequestBody req =
  case maybe Nothing restDescriptionMethodRequestRef req of
    (Just ref) -> do
      tell [DataImport $ Import ref]
      pure . pure $ ref
    _ -> pure Nothing

createResponseBody
  :: MonadThrow m
  => Maybe RestDescriptionMethodResponse
  -> GenData m (Maybe Text)
createResponseBody resp =
  case maybe Nothing restDescriptionMethodResponseRef resp of
    (Just ref) -> do
      tell [DataImport $ Import ref]
      pure . pure $ ref
    _ -> pure Nothing

createReqBody :: Bool -> Maybe Text -> [Text]
createReqBody False (Just ref) = ["  ReqBody '[JSON] " <> ref <> "." <> ref]
createReqBody True (Just ref) =
  ["  ReqBody '[JSON] (Maybe " <> ref <> "." <> ref <> ")"]
createReqBody _ _ = []

createVerb :: Text -> Maybe Text -> [Text]
createVerb method resp = case resp of
  (Just ref) -> [m <> " '[JSON] " <> ref <> "." <> ref]
  _          -> [m <> "NoContent '[JSON] NoContent"]
  where m = "  " <> T.toTitle method
