{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           Prelude                        ( putStrLn
                                                , print
                                                )
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
runCommand (Opts.ListCommand a) = do
  ret <- run $ list name preferred
  put ret
 where
  name = case Opts.name a of
    ""  -> Nothing
    n@_ -> Just n
  preferred = if Opts.preferred a then Just True else Nothing
runCommand (Opts.GetRestCommand a) = do
  ret <- run $ getRest (Opts.api a) (Opts.version a)
  -- put ret
  either (error . show) (gen "dest") ret

put :: (Show a) => Either ClientError a -> IO ()
put (Right val) = print val
put (Left  err) = putStrLn $ "Error: " ++ show err
