{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Discovery
  ( list
  , getRest
  , run
  )
where

import           RIO
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.API
import           Servant.Client                 ( BaseUrl(..)
                                                , Scheme(Https)
                                                , ClientM
                                                , ClientError
                                                , client
                                                , mkClientEnv
                                                , runClientM
                                                )
import           Data.Aeson                     ( Value )
import           Discovery.DirectoryList        ( DirectoryList )
import           Discovery.RestDescription      ( RestDescription )

type Name = Text
type Preferred = Bool
type Api = Text
type Version = Text

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "www.googleapis.com" 443 ""

type API
     = "discovery" :> "v1" :> "apis"
       :> QueryParam "name" Name
       :> QueryParam "preferred" Preferred
       :> Get '[JSON] DirectoryList
  :<|> "discovery" :> "v1" :> "apis" :> Capture "api" Api :> Capture "version" Version :> "rest"
       :> Get '[JSON] RestDescription

api :: Proxy API
api = Proxy

list :: Maybe Name -> Maybe Preferred -> ClientM DirectoryList
getRest :: Api -> Version -> ClientM RestDescription
(list :<|> getRest) = client api

run :: ClientM a -> IO (Either ClientError a)
run client = do
  manager <- newManager tlsManagerSettings
  runClientM client (mkClientEnv manager baseUrl)
