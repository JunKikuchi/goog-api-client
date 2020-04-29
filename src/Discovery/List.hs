{-# LANGUAGE DeriveGeneric #-}
module Discovery.List where

import           GHC.Generics
import           RIO
import           Data.Aeson
import           Discovery.List.Item            ( Item )

data List = List
  { kind :: Text
  , discoveryVersion :: Text
  , items :: [Item]
  } deriving (Show, Generic)

instance FromJSON List
