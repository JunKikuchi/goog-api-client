{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           RIO.Text                      as T
import           Prelude                        ( print
                                                , putStrLn
                                                )
import qualified Discovery                     as D
import qualified Discovery.DirectoryList       as DL
import qualified CodeGen                       as CD
import qualified Options                       as Opts

main :: IO ()
main = do
  opts <- Opts.parseOpts
  print opts
  runCommand opts

runCommand :: Opts.Commands -> IO ()
runCommand (Opts.GenAllCommand a) = do
  resp    <- D.run $ D.list optName optPreferred
  dirList <- either (error . show) pure resp
  items   <- CD.get DL.directoryListItems "directoryListItems" dirList
  forM_ items $ \item -> do
    name <- CD.get DL.directoryItemName "directoryItemName" item
    ver  <- CD.get DL.directoryItemVersion "directoryItemVersion" item
    putStrLn
      .  T.unpack
      $  "\n\nGenerate(name="
      <> name
      <> ", version="
      <> ver
      <> ")"
    gen name ver optDist
 where
  optName = case Opts.name a of
    ""  -> Nothing
    n@_ -> Just n
  optPreferred = if Opts.preferred a then Just True else Nothing
  optDist      = T.unpack $ Opts.genAllDist a
runCommand (Opts.GenApiCommand a) = gen optName optVer optDist
 where
  optName = Opts.api a
  optVer  = Opts.version a
  optDist = T.unpack $ Opts.genApiDist a

gen :: D.Api -> D.Version -> CD.Dist -> IO ()
gen name ver dist = do
  resp <- D.run $ D.getRest name ver
  either (error . show) (CD.gen dist) resp
