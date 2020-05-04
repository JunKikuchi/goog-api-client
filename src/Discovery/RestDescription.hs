{-# LANGUAGE OverloadedStrings #-}
module Discovery.RestDescription where

import           RIO
import           Data.Aeson                     ( FromJSON(..)
                                                , (.:?)
                                                , withObject
                                                )
import qualified JSON.Schema                   as JSON

type Key = Text

-- https://developers.google.com/discovery/v1/reference/apis/getRest

data RestDescription
  = RestDescription
  { restDescriptionKind :: Maybe Text -- "discovery#restDescription"
  , restDescriptionDiscoveryVersion :: Maybe Text -- "v1"
  , restDescriptionId :: Maybe Text
  , restDescriptionName :: Maybe Text
  , restDescriptionVersion :: Maybe Text
  , restDescriptionRevision :: Maybe Text
  , restDescriptionTitle :: Maybe Text
  , restDescriptionDescription :: Maybe Text
  , restDescriptionIcons :: Maybe RestDescriptionIcons
  , restDescriptionDocumentationLink :: Maybe Text
  , restDescriptionLabels :: Maybe [Text]
  , restDescriptionProtocol :: Maybe Text
  , restDescriptionRootUrl :: Maybe Text
  , restDescriptionServicePath :: Maybe Text
  , restDescriptionBatchPath :: Maybe Text
  , restDescriptionParameters :: Maybe (Map Key RestDescriptionParameter)
  , restDescriptionAuth :: Maybe RestDescriptionAuth
  , restDescriptionFeatures :: Maybe [Text]
  , restDescriptionSchemas :: Maybe (Map Key RestDescriptionParameter)
  , restDescriptionMethods :: Maybe (Map Key RestDescriptionMethod)
  , restDescriptionResources :: Maybe (Map Key RestDescriptionResource)
  } deriving Show

instance FromJSON RestDescription where
  parseJSON = withObject "RestDescription" $ \v -> RestDescription
    <$> v .:? "kind"
    <*> v .:? "discoveryVersion"
    <*> v .:? "id"
    <*> v .:? "name"
    <*> v .:? "version"
    <*> v .:? "revision"
    <*> v .:? "title"
    <*> v .:? "description"
    <*> v .:? "icons"
    <*> v .:? "documentationLink"
    <*> v .:? "labels"
    <*> v .:? "protocol"
    <*> v .:? "rootUrl"
    <*> v .:? "servicePath"
    <*> v .:? "batchPath"
    <*> v .:? "parameters"
    <*> v .:? "auth"
    <*> v .:? "features"
    <*> v .:? "schemas"
    <*> v .:? "methods"
    <*> v .:? "resources"

data RestDescriptionIcons
  = RestDescriptionIcons
  { restDescriptionIconsX16 :: Maybe Text
  , restDescriptionIconsX32 :: Maybe Text
  } deriving Show

instance FromJSON RestDescriptionIcons where
  parseJSON = withObject "RestDescriptionIcons" $ \v -> RestDescriptionIcons
    <$> v .:? "x16"
    <*> v .:? "x32"

data RestDescriptionParameter
  = RestDescriptionParameter
  { restDescriptionParameterId :: Maybe Text

  , restDescriptionParameterType :: Maybe Text
  , restDescriptionParameterRef :: Maybe Text
  , restDescriptionParameterDescription :: Maybe Text
  , restDescriptionParameterDefault :: Maybe Text
  , restDescriptionParameterRequired :: Maybe Bool
  , restDescriptionParameterFormat :: Maybe Text
  , restDescriptionParameterPattern :: Maybe Text
  , restDescriptionParameterMinimum :: Maybe Text
  , restDescriptionParameterMaximum :: Maybe Text
  , restDescriptionParameterEnum :: Maybe [Text]
  , restDescriptionParameterEnumDescriptions :: Maybe [Text]
  , restDescriptionParameterRepeated :: Maybe Bool
  , restDescriptionParameterLocation :: Maybe Text
  , restDescriptionParameterProperties :: Maybe (Map Key JSON.Schema)
  , restDescriptionParameterAdditionalProperties :: Maybe JSON.Schema
  , restDescriptionParameterItems:: Maybe [JSON.Schema]
  , restDescriptionParameterAnnotations :: Maybe RestDescriptionParameterAnnotations
  } deriving Show

instance FromJSON RestDescriptionParameter where
  parseJSON = withObject "RestDescriptionParameter" $ \v -> RestDescriptionParameter
    <$> v .:? "id"
    <*> v .:? "type"
    <*> v .:? "$ref"
    <*> v .:? "description"
    <*> v .:? "default"
    <*> v .:? "required"
    <*> v .:? "format"
    <*> v .:? "pattern"
    <*> v .:? "minimum"
    <*> v .:? "maximum"
    <*> v .:? "enum"
    <*> v .:? "enumDescriptions"
    <*> v .:? "repeated"
    <*> v .:? "location"
    <*> v .:? "properties"
    <*> v .:? "additionalProperties"
    <*> v .:? "items"
    <*> v .:? "annotations"

newtype RestDescriptionParameterAnnotations
  = RestDescriptionParameterAnnotations
  { restDescriptionParameterAnnotationsRequired :: Maybe [Text]
  } deriving Show

instance FromJSON RestDescriptionParameterAnnotations where
  parseJSON = withObject "RestDescriptionParameterAnnotations" $ \v -> RestDescriptionParameterAnnotations
    <$> v .:? "required"

newtype RestDescriptionAuth
  = RestDescriptionAuth
  { restDescriptionAuthOAuth2 :: Maybe RestDescriptionAuthOAuth2
  } deriving Show

instance FromJSON RestDescriptionAuth where
  parseJSON = withObject "RestDescriptionAuth" $ \v -> RestDescriptionAuth
    <$> v .:? "oauth2"

newtype RestDescriptionAuthOAuth2
  = RestDescriptionAuthOAuth2
  { restDescriptionAuthOAuth2Scopes :: Maybe (Map Key RestDescriptionAuthOAuth2Scope)
  } deriving Show

instance FromJSON RestDescriptionAuthOAuth2 where
  parseJSON = withObject "RestDescriptionAuthOAuth2" $ \v -> RestDescriptionAuthOAuth2
    <$> v .:? "scopes"

newtype RestDescriptionAuthOAuth2Scope
  = RestDescriptionAuthOAuth2Scope
  { restDescriptionAuthOAuth2ScopeDescription :: Maybe Text
  } deriving Show

instance FromJSON RestDescriptionAuthOAuth2Scope where
  parseJSON = withObject "RestDescriptionAuthOAuth2Scope" $ \v -> RestDescriptionAuthOAuth2Scope
    <$> v .:? "description"

data RestDescriptionMethod
  = RestDescriptionMethod
  { restDescriptionMethodId :: Maybe Text
  , restDescriptionMethodDescription :: Maybe Text
  , restDescriptionMethodParameters :: Maybe (Map Key RestDescriptionParameter)
  , restDescriptionMethodParameterOrder :: Maybe [Text]
  , restDescriptionMethodScopes :: Maybe [Text]
  , restDescriptionMethodSupportsMediaDownload :: Maybe Bool
  , restDescriptionMethodSupportsMediaUpload :: Maybe Bool
  , restDescriptionMethodMediaUpload :: Maybe RestDescriptionMethodMediaUpload
  , restDescriptionMethodSupportsSubscription :: Maybe Bool
  , restDescriptionMethodPath :: Maybe Text
  , restDescriptionMethodHttpMethod :: Maybe Text
  , restDescriptionMethodRequest :: Maybe RestDescriptionMethodRequest
  , restDescriptionMethodResponse :: Maybe RestDescriptionMethodResponse
  } deriving Show

instance FromJSON RestDescriptionMethod where
  parseJSON = withObject "RestDescriptionMethod" $ \v -> RestDescriptionMethod
    <$> v .:? "id"
    <*> v .:? "description"
    <*> v .:? "parameters"
    <*> v .:? "parameterOrder"
    <*> v .:? "scopes"
    <*> v .:? "supportsMediaDownload"
    <*> v .:? "supportsMediaUpload"
    <*> v .:? "mediaUpload"
    <*> v .:? "supportsSubscription"
    <*> v .:? "path"
    <*> v .:? "httpMethod"
    <*> v .:? "request"
    <*> v .:? "response"

data RestDescriptionMethodMediaUpload
  = RestDescriptionMethodMediaUpload
  { restDescriptionMethodMediaUploadAccept :: Maybe [Text]
  , restDescriptionMethodMediaMaxSize :: Maybe Text
  , restDescriptionMethodMediaProtocols :: Maybe RestDescriptionMethodMediaProtocols
  } deriving Show

instance FromJSON RestDescriptionMethodMediaUpload where
  parseJSON = withObject "RestDescriptionMethodMediaUpload" $ \v -> RestDescriptionMethodMediaUpload
    <$> v .:? "accept"
    <*> v .:? "maxSize"
    <*> v .:? "protocols"

data RestDescriptionMethodMediaProtocols
  = RestDescriptionMethodMediaProtocols
  { restDescriptionMethodMediaProtocolsSimple :: Maybe RestDescriptionMethodMediaProtocolsSimple
  , restDescriptionMethodMediaProtocolsResumable :: Maybe RestDescriptionMethodMediaProtocolsResumable
  } deriving Show

instance FromJSON RestDescriptionMethodMediaProtocols where
  parseJSON = withObject "RestDescriptionMethodMediaProtocols" $ \v -> RestDescriptionMethodMediaProtocols
    <$> v .:? "simple"
    <*> v .:? "resumable"

data RestDescriptionMethodMediaProtocolsSimple
  = RestDescriptionMethodMediaProtocolsSimple
  { restDescriptionMethodMediaProtocolsSimpleMultipart :: Maybe Bool
  , restDescriptionMethodMediaProtocolsSimplePath :: Maybe Text
  } deriving Show

instance FromJSON RestDescriptionMethodMediaProtocolsSimple where
  parseJSON = withObject "RestDescriptionMethodMediaProtocolsSimple" $ \v -> RestDescriptionMethodMediaProtocolsSimple
    <$> v .:? "multipart"
    <*> v .:? "path"

data RestDescriptionMethodMediaProtocolsResumable
  = RestDescriptionMethodMediaProtocolsResumable
  { restDescriptionMethodMediaProtocolsResumableMultipart :: Maybe Bool
  , restDescriptionMethodMediaProtocolsResumablePath :: Maybe Text
  } deriving Show

instance FromJSON RestDescriptionMethodMediaProtocolsResumable where
  parseJSON = withObject "RestDescriptionMethodMediaProtocolsResumable" $ \v -> RestDescriptionMethodMediaProtocolsResumable
    <$> v .:? "multipart"
    <*> v .:? "path"

newtype RestDescriptionMethodRequest
  = RestDescriptionMethodRequest
  { restDescriptionMethodRequestRef :: Maybe Text
  } deriving Show

instance FromJSON RestDescriptionMethodRequest where
  parseJSON = withObject "RestDescriptionMethodRequest" $ \v -> RestDescriptionMethodRequest
    <$> v .:? "$ref"

newtype RestDescriptionMethodResponse
  = RestDescriptionMethodResponse
  { restDescriptionMethodResponseRef :: Maybe Text
  } deriving Show

instance FromJSON RestDescriptionMethodResponse where
  parseJSON = withObject "RestDescriptionMethodResponse" $ \v -> RestDescriptionMethodResponse
    <$> v .:? "$ref"

data RestDescriptionResource
  = RestDescriptionResource
  { restDescriptionResourceMethods :: Maybe (Map Key RestDescriptionMethod)
  , restDescriptionResourceResources :: Maybe (Map Key RestDescriptionResource)
  } deriving Show

instance FromJSON RestDescriptionResource where
  parseJSON = withObject "RestDescriptionResource" $ \v -> RestDescriptionResource
    <$> v .:? "methods"
    <*> v .:? "resources"
