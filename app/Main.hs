module Main where

import Control.Concurrent (newMVar)
import Network.Wai.Handler.Warp (run)
import Password.ServerState (loadGameWords, newServerState)
import Password.Server (app)

main :: IO ()
main = do
  gameWords <- loadGameWords
  state <- newMVar (newServerState gameWords)
  print "http://localhost:8080"
  run 8080 (app state)
