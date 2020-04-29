{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Discovery where

import           RIO
import           RIO.Text
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
import           Discovery.List                 ( List )
import           Discovery.Document             ( Document )

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "www.googleapis.com" 443 ""

type Name = Text
type Preferred = Bool

type API
     = "discovery" :> "v1" :> "apis"
       :> QueryParam "name" Name
       :> QueryParam "preferred" Preferred
       :> Get '[JSON] List
  :<|> "discovery" :> "v1" :> "apis" :> Capture "api" Text :> Capture "version" Text :> "rest"
       :> Get '[JSON] Document

api :: Proxy API
api = Proxy

list :: Maybe Name -> Maybe Preferred -> ClientM List
getRest :: Text -> Text -> ClientM Document
(list :<|> getRest) = client api

run :: ClientM a -> IO (Either ClientError a)
run client = do
  manager <- newManager tlsManagerSettings
  runClientM client (mkClientEnv manager baseUrl)
