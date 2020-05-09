{-# LANGUAGE OverloadedStrings #-}
module Options
  ( Commands(..)
  , GenAll(..)
  , GenApi(..)
  , parseOpts
  )
where

import           RIO
import           Options.Applicative

data Commands
  = GenAllCommand GenAll
  | GenApiCommand GenApi
  deriving Show

data GenAll
  = GenAll
  { name :: Text
  , preferred :: Bool
  } deriving Show

data GenApi
  = GenApi
  { api :: Text
  , version :: Text
  } deriving Show

parseOpts :: IO Commands
parseOpts = execParser (info (commands <**> helper) idm)

commands :: Parser Commands
commands = subparser (genAllCmd <> genApiCmd)
 where
  genAllCmd = command "all" $ info (genAllCommand <**> helper) allDesc
  genApiCmd = command "api" $ info (genApiCommand <**> helper) apiDesc
  allDesc   = fullDesc <> progDesc "eg. goog-api-client-gen all"
  apiDesc   = fullDesc
    <> progDesc "eg. goog-api-client-gen api --api storage --version v1"

genAllCommand :: Parser Commands
genAllCommand = GenAllCommand <$> genAll

genApiCommand :: Parser Commands
genApiCommand = GenApiCommand <$> genApi

genAll :: Parser GenAll
genAll =
  GenAll
    <$> strOption
          (  long "name"
          <> metavar "API"
          <> help "Only include APIs with the given name."
          <> value ""
          )
    <*> switch
          (  long "preferred"
          <> help "Return only the preferred version of an API."
          )

genApi :: Parser GenApi
genApi =
  GenApi
    <$> strOption (long "api" <> metavar "API" <> help "The name of the API.")
    <*> strOption
          (long "version" <> metavar "VERSION" <> help "The version of the API."
          )
