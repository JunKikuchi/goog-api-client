{-# LANGUAGE OverloadedStrings #-}
module Discovery.DirectoryList
  ( DirectoryList(..)
  , DirectoryItem(..)
  , DirectoryItemIcons(..)
  )
where

import           RIO
import           Data.Aeson                     ( FromJSON(..)
                                                , (.:?)
                                                , withObject
                                                )

-- https://developers.google.com/discovery/v1/reference/apis/list

data DirectoryList
  = DirectoryList
  { directoryListKind :: Maybe Text -- "discovery#directoryList"
  , directoryListDiscoveryVersion :: Maybe Text -- "v1"
  , directoryListItems :: Maybe [DirectoryItem]
  } deriving Show

instance FromJSON DirectoryList where
  parseJSON = withObject "DirectoryList" $ \v -> DirectoryList
    <$> v .:? "kind"
    <*> v .:? "discoveryVersion"
    <*> v .:? "items"

data DirectoryItem
  = DirectoryItem
  { directoryItemKind :: Maybe Text -- "discovery#directoryItem"
  , directoryItemId :: Maybe Text
  , directoryItemName :: Maybe Text
  , directoryItemVersion :: Maybe Text
  , directoryItemTitle :: Maybe Text
  , directoryItemDescription :: Maybe Text
  , directoryItemDiscoveryRestUrl :: Maybe Text
  , directoryItemDiscoveryLink :: Maybe Text
  , directoryItemIcons :: Maybe DirectoryItemIcons
  , directoryItemDocumentationLink :: Maybe Text
  , directoryItemLabel :: Maybe [Text]
  , directoryItemPreferred :: Maybe Bool
  } deriving Show

instance FromJSON DirectoryItem where
  parseJSON = withObject "DirectoryItem" $ \v -> DirectoryItem
    <$> v .:? "kind"
    <*> v .:? "id"
    <*> v .:? "name"
    <*> v .:? "version"
    <*> v .:? "title"
    <*> v .:? "description"
    <*> v .:? "discoveryRestUrl"
    <*> v .:? "discoveryLink"
    <*> v .:? "icons"
    <*> v .:? "documentationLink"
    <*> v .:? "labels"
    <*> v .:? "preferred"

data DirectoryItemIcons
  = DirectoryItemIcons
  { directoryItemIconsX16 :: Maybe Text
  , directoryItemIconsX32 :: Maybe Text
  } deriving Show

instance FromJSON DirectoryItemIcons where
  parseJSON = withObject "DirectoryItemIcons" $ \v -> DirectoryItemIcons
    <$> v .:? "x16"
    <*> v .:? "x32"
