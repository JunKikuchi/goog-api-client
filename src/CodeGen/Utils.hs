{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Utils where

import           RIO
import           RIO.Text                      as T

get :: Applicative f => (t -> Maybe a) -> Text -> t -> f a
get f s desc = maybe (error err) pure (f desc)
  where err = T.unpack $ T.intercalate " " ["failed to get", s]
