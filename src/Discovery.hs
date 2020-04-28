{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Discovery where

import           RIO
import           RIO.Text
import           Servant.API
import           Servant.Client
import           Discovery.List                 ( List )

type Desc = Text

type Discovery
     = "discovery" :> "v1" :> "apis" :>                                                           Get '[JSON] List
  :<|> "discovery" :> "v1" :> "apis" :> Capture "api" Text :> Capture "version" Text :> "rest" :> Get '[PlainText] Desc

discovery :: Proxy Discovery
discovery = Proxy

list :: ClientM List
getRest :: Text -> Text -> ClientM Desc
(list :<|> getRest) = client discovery
