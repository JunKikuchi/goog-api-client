{-# LANGUAGE OverloadedStrings #-}
module Discovery.RestDescription
  ( RestDescription(..)
  , RestDescriptionIcons(..)
  , RestDescriptionParameters
  , RestDescriptionSchemas
  , RestDescriptionMethods
  , RestDescriptionResources
  , RestDescriptionAuth(..)
  , RestDescriptionAuthOAuth2(..)
  , RestDescriptionAuthOAuth2Scope(..)
  , RestDescriptionMethod(..)
  , RestDescriptionMethodMediaUpload(..)
  , RestDescriptionMethodMediaProtocols(..)
  , RestDescriptionMethodMediaProtocolsSimple(..)
  , RestDescriptionMethodMediaProtocolsResumable(..)
  , RestDescriptionMethodRequest(..)
  , RestDescriptionMethodResponse(..)
  , RestDescriptionResource(..)
  , module Discovery.RestDescription.Schema
  )
where

import           RIO
import           Data.Aeson                     ( (.:?) )
import qualified Data.Aeson                    as Aeson
import           Discovery.RestDescription.Schema

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
  , restDescriptionParameters :: Maybe RestDescriptionParameters
  , restDescriptionAuth :: Maybe RestDescriptionAuth
  , restDescriptionFeatures :: Maybe [Text]
  , restDescriptionSchemas :: Maybe RestDescriptionSchemas
  , restDescriptionMethods :: Maybe RestDescriptionMethods
  , restDescriptionResources :: Maybe RestDescriptionResources
  } deriving Show

instance Aeson.FromJSON RestDescription where
  parseJSON = Aeson.withObject "RestDescription" $ \v -> RestDescription
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

instance Aeson.FromJSON RestDescriptionIcons where
  parseJSON = Aeson.withObject "RestDescriptionIcons" $ \v -> RestDescriptionIcons
    <$> v .:? "x16"
    <*> v .:? "x32"

type RestDescriptionParameters = Map Key Schema
type RestDescriptionSchemas    = Map Key Schema
type RestDescriptionMethods    = Map Key RestDescriptionMethod
type RestDescriptionResources  = Map Key RestDescriptionResource

newtype RestDescriptionAuth
  = RestDescriptionAuth
  { restDescriptionAuthOAuth2 :: Maybe RestDescriptionAuthOAuth2
  } deriving Show

instance Aeson.FromJSON RestDescriptionAuth where
  parseJSON = Aeson.withObject "RestDescriptionAuth" $ \v -> RestDescriptionAuth
    <$> v .:? "oauth2"

newtype RestDescriptionAuthOAuth2
  = RestDescriptionAuthOAuth2
  { restDescriptionAuthOAuth2Scopes :: Maybe (Map Key RestDescriptionAuthOAuth2Scope)
  } deriving Show

instance Aeson.FromJSON RestDescriptionAuthOAuth2 where
  parseJSON = Aeson.withObject "RestDescriptionAuthOAuth2" $ \v -> RestDescriptionAuthOAuth2
    <$> v .:? "scopes"

newtype RestDescriptionAuthOAuth2Scope
  = RestDescriptionAuthOAuth2Scope
  { restDescriptionAuthOAuth2ScopeDescription :: Maybe Text
  } deriving Show

instance Aeson.FromJSON RestDescriptionAuthOAuth2Scope where
  parseJSON = Aeson.withObject "RestDescriptionAuthOAuth2Scope" $ \v -> RestDescriptionAuthOAuth2Scope
    <$> v .:? "description"

data RestDescriptionMethod
  = RestDescriptionMethod
  { restDescriptionMethodId :: Maybe Text
  , restDescriptionMethodDescription :: Maybe Text
  , restDescriptionMethodParameters :: Maybe RestDescriptionParameters
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

instance Aeson.FromJSON RestDescriptionMethod where
  parseJSON = Aeson.withObject "RestDescriptionMethod" $ \v -> RestDescriptionMethod
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

instance Aeson.FromJSON RestDescriptionMethodMediaUpload where
  parseJSON = Aeson.withObject "RestDescriptionMethodMediaUpload" $ \v -> RestDescriptionMethodMediaUpload
    <$> v .:? "accept"
    <*> v .:? "maxSize"
    <*> v .:? "protocols"

data RestDescriptionMethodMediaProtocols
  = RestDescriptionMethodMediaProtocols
  { restDescriptionMethodMediaProtocolsSimple :: Maybe RestDescriptionMethodMediaProtocolsSimple
  , restDescriptionMethodMediaProtocolsResumable :: Maybe RestDescriptionMethodMediaProtocolsResumable
  } deriving Show

instance Aeson.FromJSON RestDescriptionMethodMediaProtocols where
  parseJSON = Aeson.withObject "RestDescriptionMethodMediaProtocols" $ \v -> RestDescriptionMethodMediaProtocols
    <$> v .:? "simple"
    <*> v .:? "resumable"

data RestDescriptionMethodMediaProtocolsSimple
  = RestDescriptionMethodMediaProtocolsSimple
  { restDescriptionMethodMediaProtocolsSimpleMultipart :: Maybe Bool
  , restDescriptionMethodMediaProtocolsSimplePath :: Maybe Text
  } deriving Show

instance Aeson.FromJSON RestDescriptionMethodMediaProtocolsSimple where
  parseJSON = Aeson.withObject "RestDescriptionMethodMediaProtocolsSimple" $ \v -> RestDescriptionMethodMediaProtocolsSimple
    <$> v .:? "multipart"
    <*> v .:? "path"

data RestDescriptionMethodMediaProtocolsResumable
  = RestDescriptionMethodMediaProtocolsResumable
  { restDescriptionMethodMediaProtocolsResumableMultipart :: Maybe Bool
  , restDescriptionMethodMediaProtocolsResumablePath :: Maybe Text
  } deriving Show

instance Aeson.FromJSON RestDescriptionMethodMediaProtocolsResumable where
  parseJSON = Aeson.withObject "RestDescriptionMethodMediaProtocolsResumable" $ \v -> RestDescriptionMethodMediaProtocolsResumable
    <$> v .:? "multipart"
    <*> v .:? "path"

newtype RestDescriptionMethodRequest
  = RestDescriptionMethodRequest
  { restDescriptionMethodRequestRef :: Maybe Text
  } deriving Show

instance Aeson.FromJSON RestDescriptionMethodRequest where
  parseJSON = Aeson.withObject "RestDescriptionMethodRequest" $ \v -> RestDescriptionMethodRequest
    <$> v .:? "$ref"

newtype RestDescriptionMethodResponse
  = RestDescriptionMethodResponse
  { restDescriptionMethodResponseRef :: Maybe Text
  } deriving Show

instance Aeson.FromJSON RestDescriptionMethodResponse where
  parseJSON = Aeson.withObject "RestDescriptionMethodResponse" $ \v -> RestDescriptionMethodResponse
    <$> v .:? "$ref"

data RestDescriptionResource
  = RestDescriptionResource
  { restDescriptionResourceMethods :: Maybe (Map Key RestDescriptionMethod)
  , restDescriptionResourceResources :: Maybe (Map Key RestDescriptionResource)
  } deriving Show

instance Aeson.FromJSON RestDescriptionResource where
  parseJSON = Aeson.withObject "RestDescriptionResource" $ \v -> RestDescriptionResource
    <$> v .:? "methods"
    <*> v .:? "resources"
