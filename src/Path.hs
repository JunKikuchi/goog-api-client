module Path
  ( Path.parse
  , module Path.Types
  )
where

import           RIO
import           Text.Megaparsec               as Parsec
import           Path.Parser
import           Path.Types

parse :: Text -> Either (ParseErrorBundle Text Void) Path
parse = Parsec.parse path ""
