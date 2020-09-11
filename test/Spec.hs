module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import qualified Network.WebSockets as WS
import Password.Server (mkApp)
import Password.ServerState
import Test.Hspec

main :: IO ()
main = do
  _ <- putStrLn "Running unit tests..."
  _ <- units
  _ <- putStrLn "\nRunning e2e tests...\n"
  _ <- e2eMain
  return ()

units :: IO ()
units = hspec $ do
  let _words = GameWords {easy = ["lambda"], medium = [], hard = []}
  let game =
        PasswordGame
          { teamA = ["one", "two"],
            teamB = ["three", "four"],
            teamAClueGiver = "one",
            teamBClueGiver = "three",
            teamAGuesser = "two",
            teamBGuesser = "four",
            word = "lambda",
            clues = ["haskell"],
            guesses = [],
            teamAScore = 1,
            teamBScore = 1,
            possession = TeamA
          }
  describe "submitClue" $ do
    it "returns err if clue is word" $ do
      result <- submitClue "one" (word game) _words game
      result `shouldBe` Left "you tried to submit the word as a clue!"
  describe "guessWord" $ do
    it "returns err if not a guesser" $ do
      result <- guessWord "one" "guess" _words game
      result `shouldBe` Left "You are not a guesser!"
    it "only adds guess if wrong" $ do
      result <- guessWord "two" "wrong" _words game
      result `shouldBe` Right game {guesses = ["wrong"]}
    it "increments score and resets for next round if correct" $ do
      result <- guessWord "two" "lambda" _words game
      result
        `shouldBe` Right
          game
            { teamAScore = 11,
              teamAClueGiver = "two",
              teamBClueGiver = "four",
              teamAGuesser = "one",
              teamBGuesser = "three",
              guesses = [],
              clues = [],
              possession = TeamB
            }
    it "points are earned by number of guesses used" $ do
      result <-
        guessWord
          "four"
          "lambda"
          _words
          game
            { guesses = ["curry"],
              clues = ["haskell", "function"]
            }
      result
        `shouldBe` Right
          game
            { teamBScore = 10,
              teamAClueGiver = "two",
              teamBClueGiver = "four",
              teamAGuesser = "one",
              teamBGuesser = "three",
              guesses = [],
              clues = [],
              possession = TeamB
            }

-- E2E

testClient :: String -> WS.ClientApp ()
testClient name conn = do
  putStrLn $ "[testClient(" ++ name ++ ")] Started"
  msg1 <- WS.receiveData conn
  putStrLn $ "[testClient(" ++ name ++ ")] received message:"
  T.putStrLn msg1
  WS.sendTextData conn $ T.pack $ "{\"type\":\"player-name-updated\",\"payload\":\"" ++ name ++ "\"}"
  msg2 <- WS.receiveData conn
  putStrLn $ "[testClient(" ++ name ++ ")] received message:"
  T.putStrLn msg2

runTestClients :: IO ()
runTestClients = do
  one <- myForkIO "client one" $ withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" (testClient "one")
  two <- myForkIO "client two" $ withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" (testClient "two")
  three <- myForkIO "client three" $ withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" (testClient "three")
  four <- myForkIO "client four" $ withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" (testClient "four")
  _ <- takeMVar one
  _ <- takeMVar two
  _ <- takeMVar three
  _ <- takeMVar four
  return ()

onServerUp :: MVar () -> IO ()
onServerUp mvar = do
  putStrLn "server is up!"
  putMVar mvar ()

e2eMain :: IO ()
e2eMain = do
  app <- mkApp
  serverReadyMVar <- newEmptyMVar
  let serverSettings = setPort 8080 $ setBeforeMainLoop (onServerUp serverReadyMVar) defaultSettings
  _ <- myForkIO "server" $ runSettings serverSettings app
  _ <- takeMVar serverReadyMVar
  putStrLn "[main] Started the server"
  clientMVar <- myForkIO "runTestClients" runTestClients
  putStrLn "[main] Started the clients..."
  _ <- takeMVar clientMVar
  putStrLn "[main] All done."

myForkIO :: String -> IO () -> IO (MVar ())
myForkIO label io = do
  mvar <- newEmptyMVar
  _ <-
    forkFinally
      io
      ( \result -> do
          case result of
            Left _ -> do
              _ <- putStrLn (label ++ " exited with failure.\n" ++ show result)
              return ()
            Right _ -> do
              _ <- putStrLn (label ++ " exited ok.")
              return ()
          putMVar mvar ()
      )
  return mvar
