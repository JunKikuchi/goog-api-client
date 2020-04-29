{-# LANGUAGE DeriveGeneric #-}
module Discovery.Document where

import           GHC.Generics
import           RIO
import qualified RIO.Map                       as Map
import           RIO.Text
import           Data.Aeson

data Document = Document
  { kind :: Text
  , discoveryVersion :: Text
  , id :: Text
  , name :: Text
  , version :: Text
  , revision :: Text
  , title :: Text
  , description :: Text
  , icons :: Icons
  , documentationLink :: Text
  , labels :: [Text]
  , protocol :: Text
  , rootUrl :: Text
  -- , parameters :: Map Text Parameter
  -- , auto :: Auth
  , features :: [Text]
  -- , schemas :: Schemas
  -- , methods :: Methods
  , baseUrl :: Text
  , basePath :: Text
  , servicePath :: Text
  , batchPath :: Text
  -- , resources :: Resources
  } deriving (Show, Generic)

instance FromJSON Document

data Icons
  = Icons
  { x16 :: Text
  , x32 :: Text
  } deriving (Show, Generic)

instance FromJSON Icons

data Auth
  = Auth
