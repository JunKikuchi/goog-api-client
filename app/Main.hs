{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           RIO.Text                      as T
import           Prelude                        ( print )
import           Servant.Client                 ( ClientError )
import           Discovery                      ( list
                                                , getRest
                                                , run
                                                )
import           CodeGen                        ( gen )
import qualified Options                       as Opts

main :: IO ()
main = do
  opts <- Opts.parseOpts
  print opts
  runCommand opts

runCommand :: Opts.Commands -> IO ()
runCommand (Opts.GenAllCommand a) = do
  ret <- run $ list name preferred
  put ret
 where
  name = case Opts.name a of
    ""  -> Nothing
    n@_ -> Just n
  preferred = if Opts.preferred a then Just True else Nothing
runCommand (Opts.GenApiCommand a) = do
  ret <- run $ getRest (Opts.api a) (Opts.version a)
  let dist = T.unpack $ Opts.genApiDist a
  either (error . show) (gen dist) ret

put :: (Show a) => Either ClientError a -> IO ()
put = either (print . ("Error: " ++) . show) print
