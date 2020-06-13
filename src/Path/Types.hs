module Path.Types where

import           RIO

newtype Path
  = Path
  { pathSegments :: [Segment]
  } deriving (Show, Eq)

type Segment = [Template]

data Template
  = Literal Text
  | Expression (Maybe Operator) Text
  deriving (Show, Eq)

data Operator
  = Reserved
  | Fragment
  deriving (Show, Eq)
