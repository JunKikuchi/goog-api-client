module Path.Types where

import           RIO

newtype Path
  = Path
  { pathSegments :: [Segment]
  } deriving (Show, Eq)

type Segment = [Template]

data Template
  = Literal Text
  | Expression (Maybe Operator) (Maybe Modifier) Text
  deriving (Show, Eq)

data Operator
  = Reserved
  | Fragment
  | PathSegment
  deriving (Show, Eq)

data Modifier
  = Explode
  | Prefix Int
  deriving (Show, Eq)
