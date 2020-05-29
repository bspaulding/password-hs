module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop)
import Password.Server (mkApp)
import qualified Network.WebSockets as WS

testClient :: String -> WS.ClientApp ()
testClient name conn = do
  print $ "[testClient(" ++ name ++ ")] Started"
  msg <- WS.receiveData conn
  print $ "[testClient(" ++ name ++ ")] received message:"
  T.putStrLn msg
  WS.sendTextData conn $ T.pack $ "{\"type\":\"player-name-updated\",\"payload\":\"" ++ name ++ "\"}"
  msg <- WS.receiveData conn
  print $ "[testClient(" ++ name ++ ")] received message:"
  T.putStrLn msg

runTestClients :: IO ()
runTestClients = do
  one <- myForkIO $ withSocketsDo $ WS.runClient "localhost" 3000 "/" (testClient "one")
  two <- myForkIO $ withSocketsDo $ WS.runClient "localhost" 3000 "/" (testClient "two")
  three <- myForkIO $ withSocketsDo $ WS.runClient "localhost" 3000 "/" (testClient "three")
  four <- myForkIO $ withSocketsDo $ WS.runClient "localhost" 3000 "/" (testClient "four")
  _ <- takeMVar one
  _ <- takeMVar two
  _ <- takeMVar three
  _ <- takeMVar four
  return ()

onServerUp :: MVar () -> IO ()
onServerUp mvar = do
  print "server is up!"
  putMVar mvar ()

main :: IO ()
main = do
  app <- mkApp
  serverReadyMVar <- newEmptyMVar
  let serverSettings = setBeforeMainLoop (onServerUp serverReadyMVar) defaultSettings
  _ <- myForkIO $ runSettings serverSettings app
  _ <- takeMVar serverReadyMVar
  print "[main] Started the server"
  clientMVar <- myForkIO runTestClients
  print "[main] Started the client..."
  _ <- takeMVar clientMVar
  print "[main] All done."

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\result -> do
    case result of
      Left _ -> print result
      Right _ -> return ()
    putMVar mvar ())
  return mvar
