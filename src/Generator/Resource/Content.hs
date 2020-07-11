{-# LANGUAGE OverloadedStrings #-}
module Generator.Resource.Content
  ( createContent
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Writer                     ( tell )
import           Discovery.RestDescription
import           Generator.Types
import           Generator.Util
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
  -> GenImport m (ApiName, Text)
createContent commonParams method = do
  methodId   <- get restDescriptionMethodId "method id" method
  path       <- get restDescriptionMethodPath "method path" method
  httpMethod <- get restDescriptionMethodHttpMethod "method httpMethod" method
  let apiName  = toCamelName methodId
      pathName = unTitle $ apiName <> "Path"
  cpath         <- createPath apiName params pathName path paramOrder
  captures      <- createCapture pathName
  queries       <- createQueryParam params
  commonQueries <- createQueryParam commonParams
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
      content = desc <> apiType <> "\n\n" <> cpath
  pure (apiName, content)
 where
  params          = fromMaybe Map.empty $ restDescriptionMethodParameters method
  paramOrder      = fromMaybe [] $ restDescriptionMethodParameterOrder method
  upload = fromMaybe False $ restDescriptionMethodSupportsMediaUpload method
  uploadTypeQuery = [ "  QueryParam \"uploadType\" RIO.Text" | upload ]

createPath
  :: MonadThrow m
  => ApiName
  -> RestDescriptionParameters
  -> Text
  -> Text
  -> [Text]
  -> GenImport m Text
createPath apiName params pathName path paramOrder = do
  tell (Set.singleton ImportPrelude)
  paths    <- either throwM pure $ Path.parse path
  pathArgs <- createPathParams paths
  let argNames = foldr f [] . join . pathSegments $ paths
  types <- createPathTypes pathParams argNames
  let desc =
        descContent 0
          .  Just
          $  "Create CaptureAll path parameter for "
          <> apiName
          <> (if null paramOrder
               then ""
               else "\n\nArgs: " <> T.intercalate ", " paramOrder
             )
      functionType = desc <> pathName <> " :: " <> T.intercalate
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

createReqBody :: Bool -> Maybe Text -> [Text]
createReqBody False (Just ref) = ["  ReqBody '[JSON] " <> ref <> "." <> ref]
createReqBody _     _          = []

createVerb :: Text -> Maybe Text -> [Text]
createVerb method resp = case resp of
  (Just ref) -> [m <> " '[JSON] " <> ref <> "." <> ref]
  _          -> [m <> "NoContent '[JSON] NoContent"]
  where m = "  " <> T.toTitle method
