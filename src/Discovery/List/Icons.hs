{-# LANGUAGE DeriveGeneric #-}
module Discovery.List.Icons where

import           GHC.Generics
import           RIO
import           Data.Aeson

data Icons = Icons
   { x16 :: Text
   , x32 :: Text
   } deriving (Show, Generic)

instance FromJSON Icons
