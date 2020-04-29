{-# LANGUAGE DeriveGeneric #-}
module Discovery.Desc where

import           GHC.Generics
import           RIO
import           RIO.Text
import           Data.Aeson

data Desc = Desc
  { kind :: Text
  , discoveryVersion :: Text
  } deriving (Show, Generic)

instance FromJSON Desc
