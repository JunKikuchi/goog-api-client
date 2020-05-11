{-# LANGUAGE OverloadedStrings #-}
module CodeGen.Util where

import           RIO
import qualified RIO.Char                      as C
import qualified RIO.Directory                 as Dir
import qualified RIO.Text                      as T

get :: Applicative f => (t -> Maybe a) -> Text -> t -> f a
get f s desc = maybe (error err) pure (f desc)
  where err = T.unpack $ T.intercalate " " ["failed to get", s]

toTitle :: Text -> Text
toTitle = applyHead C.toUpper

unTitle :: Text -> Text
unTitle = applyHead C.toLower

applyHead :: (Char -> Char) -> Text -> Text
applyHead f text = maybe text (\(c, t) -> T.cons (f c) t) (T.uncons text)

withDir :: FilePath -> IO a -> IO a
withDir dir action = do
  Dir.createDirectoryIfMissing True dir
  Dir.withCurrentDirectory dir action
