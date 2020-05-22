module Main where

import Network.Wai.Handler.Warp (run)
import Password.Server (mkApp)

main :: IO ()
main = do
  app <- mkApp
  print "http://localhost:8080"
  run 8080 app
