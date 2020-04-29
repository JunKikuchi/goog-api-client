{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.Client                 ( runClientM
                                                , mkClientEnv
                                                )
import           Prelude                        ( putStrLn
                                                , print
                                                )
import           Discovery                      ( baseUrl
                                                , list
                                                )

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  res     <- runClientM (list (Just "firestore") (Just False))
                        (mkClientEnv manager baseUrl)
  case res of
    Left  err   -> putStrLn $ "Error: " ++ show err
    Right items -> print items
