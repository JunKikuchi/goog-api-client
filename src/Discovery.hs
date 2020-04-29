{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Discovery where

import           RIO
import           RIO.Text
import           Servant.API
import           Servant.Client
import           Discovery.List                 ( List )

type Desc = Text
type Name = Text
type Preferred = Bool

type Discovery
     = "discovery" :> "v1" :> "apis"
       :> QueryParam "name" Name
       :> QueryParam "preferred" Preferred
       :> Get '[JSON] List
  :<|> "discovery" :> "v1" :> "apis" :> Capture "api" Text :> Capture "version" Text :> "rest"
       :> Get '[PlainText] Desc

discovery :: Proxy Discovery
discovery = Proxy

list :: Maybe Name -> Maybe Preferred -> ClientM List
getRest :: Text -> Text -> ClientM Desc

(list :<|> getRest) = client discovery
