module Main where

import Control.Concurrent (newMVar)
import Network.Wai.Handler.Warp (run)
import Password.ServerState (newServerState)
import Password.Server (app)

main :: IO ()
main = do
  initialServerState <- newServerState
  state <- newMVar initialServerState
  print "http://localhost:8080"
  run 8080 (app state)
