{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Discovery where

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

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "www.googleapis.com" 443 ""

type API
     = "discovery" :> "v1" :> "apis"
       :> QueryParam "name" Text
       :> QueryParam "preferred" Bool
       :> Get '[JSON] Value
  :<|> "discovery" :> "v1" :> "apis" :> Capture "api" Text :> Capture "version" Text :> "rest"
       :> Get '[JSON] Value

api :: Proxy API
api = Proxy

list :: Maybe Text -> Maybe Bool -> ClientM Value
getRest :: Text -> Text -> ClientM Value
(list :<|> getRest) = client api

run :: ClientM a -> IO (Either ClientError a)
run client = do
  manager <- newManager tlsManagerSettings
  runClientM client (mkClientEnv manager baseUrl)
