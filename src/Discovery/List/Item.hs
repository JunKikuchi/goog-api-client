{-# LANGUAGE DeriveGeneric #-}
module Discovery.List.Item where

import           GHC.Generics
import           RIO
import           RIO.Text
import           Data.Aeson
import           Discovery.List.Icons           ( Icons )

data Item = Item
  { kind :: Text
  , id :: Text
  , name :: Text
  , version :: Text
  , title :: Text
  , description :: Text
  , discoveryRestUrl :: Text
  , discoveryLink :: Maybe Text
  , icons :: Icons
  , documentationLink :: Text
  , labels :: Maybe [Text]
  , preferred :: Bool
  } deriving (Show, Generic)

instance FromJSON Item
