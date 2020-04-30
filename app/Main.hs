{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           Prelude                        ( putStrLn
                                                , print
                                                )
import           Discovery                      ( list
                                                , getRest
                                                , run
                                                )
import qualified Options                       as Opts

main :: IO ()
main = do
  opts <- Opts.parseOpts
  print opts

  runCommand opts

runCommand :: Opts.Commands -> IO ()
runCommand (Opts.ListCommand a) = do
  res <- run $ list name preferred
  case res of
    Left  err   -> putStrLn $ "Error: " ++ show err
    Right items -> print items
 where
  name = case Opts.name a of
    ""  -> Nothing
    n@_ -> Just n
  preferred = if Opts.preferred a then Just True else Nothing
runCommand (Opts.GetRestCommand a) = do
  res <- run $ getRest (Opts.api a) (Opts.version a)
  case res of
    Left  err   -> putStrLn $ "Error: " ++ show err
    Right items -> print items
