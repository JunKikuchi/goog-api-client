module Options where

import           RIO
import           Options.Applicative

data Commands
  = ListCommand List
  | GetRestCommand GetRest
  deriving Show

data List
  = List
  { name :: Maybe Text
  , preferred :: Bool
  } deriving Show

data GetRest
  = GetRest
  { api :: Text
  , version :: Text
  } deriving Show

parseOpts :: IO Commands
parseOpts = execParser (info (commands <**> helper) idm)

commands :: Parser Commands
commands = subparser (l <> g)
 where
  l     = command "list" $ info (listCommand <**> helper) ldesc
  g     = command "getRest" $ info (getRestCommand <**> helper) gdesc
  ldesc = fullDesc <> progDesc "eg. goog-api-client-exe list"
  gdesc = fullDesc
    <> progDesc "eg. goog-api-client-exe getRest --api storage --version v1"

listCommand :: Parser Commands
listCommand = ListCommand <$> list

getRestCommand :: Parser Commands
getRestCommand = GetRestCommand <$> getRest

list :: Parser List
list =
  List
    <$> option
          auto
          (  long "name"
          <> metavar "API"
          <> help "Only include APIs with the given name."
          <> value Nothing
          )
    <*> switch
          (  long "preferred"
          <> help "Return only the preferred version of an API."
          )

getRest :: Parser GetRest
getRest =
  GetRest
    <$> option auto (long "api" <> metavar "API" <> help "The name of the API.")
    <*> option
          auto
          (long "version" <> metavar "VERSION" <> help "The version of the API."
          )
