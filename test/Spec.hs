module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop)
import qualified Network.WebSockets as WS
import Password.Server (mkApp)
import Password.ServerState
import Test.Hspec

main :: IO ()
main = hspec $ do
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

e2eMain :: IO ()
e2eMain = do
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
  _ <-
    forkFinally
      io
      ( \result -> do
          case result of
            Left _ -> print result
            Right _ -> return ()
          putMVar mvar ()
      )
  return mvar
