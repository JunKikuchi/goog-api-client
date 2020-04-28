{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RIO
import           Discovery                      ( list )
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Servant.Client                 ( BaseUrl(..)
                                                , Scheme(..)
                                                , runClientM
                                                , mkClientEnv
                                                )
import           Prelude                        ( putStrLn
                                                , print
                                                )

baseURL :: BaseUrl
baseURL = BaseUrl Https "www.googleapis.com" 443 ""

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  res     <- runClientM list (mkClientEnv manager baseURL)
  case res of
    Left  err   -> putStrLn $ "Error: " ++ show err
    Right items -> print items
