{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           RIO.Text                      as T
import           Prelude                        ( print
                                                , putStrLn
                                                )
import           Discovery                      ( list
                                                , getRest
                                                , run
                                                )
import qualified Discovery.DirectoryList       as DL
import           CodeGen                        ( gen )
import           CodeGen.Util                   ( get )
import qualified Options                       as Opts

main :: IO ()
main = do
  opts <- Opts.parseOpts
  print opts
  runCommand opts

runCommand :: Opts.Commands -> IO ()
runCommand (Opts.GenAllCommand a) = do
  let dist = T.unpack $ Opts.genAllDist a
  ret   <- run $ list name preferred
  dl    <- either (error . show) pure ret
  items <- get DL.directoryListItems "directoryListItems" dl
  forM_ items $ \item -> do
    n <- get DL.directoryItemName "directoryItemName" item
    v <- get DL.directoryItemVersion "directoryItemVersion" item
    putStrLn . T.unpack $ "\n\nGenerate(name=" <> n <> ", version=" <> v <> ")"
    r <- run $ getRest n v
    either (print . show) (gen dist) r
 where
  name = case Opts.name a of
    ""  -> Nothing
    n@_ -> Just n
  preferred = if Opts.preferred a then Just True else Nothing
runCommand (Opts.GenApiCommand a) = do
  let dist = T.unpack $ Opts.genApiDist a
  ret <- run $ getRest (Opts.api a) (Opts.version a)
  either (print . show) (gen dist) ret
