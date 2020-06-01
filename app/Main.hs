module Main where

import Network.Wai.Handler.Warp (run)
import Password.Server (mkApp)
import System.Environment

getPort :: IO Int
getPort = do
  sysPort <- lookupEnv "PORT"
  return $ case sysPort of
              Nothing -> 8080
              Just portStr -> read portStr :: Int

main :: IO ()
main = do
  app <- mkApp
  port <- getPort
  print $ "http://localhost:" ++ show port
  run port app
