{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_)
import Control.Exception (finally)
import Control.Monad (forever, forM_)
import Data.Aeson
import Data.Map.Internal as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.IO as T
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import qualified Password.Msg.CreateRoomResponse as CreateRoomResponse
import qualified Password.Msg.ErrorResponse as ErrorResponse
import qualified Password.Msg.JoinedRoom as JoinedRoom
import qualified Password.Msg.JoinRoomResponse as JoinRoomResponse
import qualified Password.Msg.NewWordResponse as NewWordResponse
import qualified Password.Msg.PlayerNameChanged as PlayerNameChanged
import System.Random

type RoomId = String
type ConnId = String
type Client = (ConnId, WS.Connection)

instance Show WS.Connection where
  show conn = "<wsconn>"

data ServerState =
  ServerState
    { rooms :: Map RoomId [Client]
    , roomIdByConnId :: Map ConnId RoomId
    , lobby :: [Client]
    , names :: Map ConnId String
    } deriving (Show)

newServerState :: ServerState
newServerState =
  ServerState
    { rooms = Map.empty
    , lobby = []
    , names = Map.empty
    , roomIdByConnId = Map.empty
    }

addToLobby :: Client -> ServerState -> ServerState
addToLobby client s = s { lobby = client : lobby s }

removeFromLobby :: Client -> ServerState -> ServerState
removeFromLobby client s = s { lobby = Prelude.filter ((/= fst client) . fst) (lobby s) }

removeFromRooms :: Client -> ServerState -> ServerState
removeFromRooms client s = s { rooms = Map.map (\clients -> Prelude.filter ((/= fst client) . fst) clients) (rooms s)
                             , roomIdByConnId = Map.delete (fst client) (roomIdByConnId s)
                             }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = (removeFromLobby client (removeFromRooms client s))

addToRoom :: RoomId -> Client -> ServerState -> ServerState
addToRoom roomId client s = s { rooms = insertWith (++) roomId [client] (rooms s)
                              , roomIdByConnId = insert (fst client) roomId (roomIdByConnId s)
                              }

moveClientToRoom  :: RoomId -> Client -> ServerState -> ServerState
moveClientToRoom roomId client s = addToRoom roomId client (removeFromLobby client s)

getRoomId  :: ConnId -> ServerState -> Maybe RoomId
getRoomId connId state = Map.lookup connId (roomIdByConnId state)

updatePlayerName :: Client -> String -> ServerState -> ServerState
updatePlayerName (connId, _) name s = s { names = Map.insert connId name (names s) }

playerName :: Client -> ServerState -> String
playerName (connId, _) s = Map.findWithDefault "Unknown" connId (names s)

broadcast :: RoomId -> T.Text -> ServerState -> IO ()
broadcast roomId message s = do
  T.putStrLn $ T.pack $ "broadcast to room " ++ roomId ++ ": " ++ (T.unpack message)
  forM_ (findWithDefault [] roomId (rooms s)) $ \(_, conn) -> WS.sendTextData conn message

app :: IO String -> MVar ServerState -> Application
app nextWord stateM = websocketsOr WS.defaultConnectionOptions wsApp httpApp
  where
    disconnect client = do
      modifyMVar_ stateM $ \s -> do
        putStrLn $ "Client with id '" ++ (fst client) ++ "' disconnected."
        return $ removeClient client s

    wsApp :: WS.ServerApp
    wsApp pending_conn = do
      conn <- WS.acceptRequest pending_conn
      id <- fmap UUID.toString UUID.nextRandom
      let client = (id, conn)
      modifyMVar_ stateM $ \state -> do
        putStrLn $ "Adding user " ++ id ++ " to lobby"
        return $ addToLobby client state
      flip finally (disconnect client) $ do
        WS.withPingThread conn 30 (return ()) $ do
          forever $ do
            msg <- WS.receiveData conn
            state <- readMVar stateM
            case decode msg :: Maybe (Map String String) of
              Just msgMap ->
                case (Map.lookup "type" msgMap, Map.lookup "payload" msgMap) of
                  (Just "create-room", _) -> do
                    roomId <- makeRoomId
                    modifyMVar_ stateM $ \state -> do
                      let state' = moveClientToRoom roomId client state
                      putStrLn $ "Client '" ++ id ++ "' created and joined room " ++ roomId
                      putStrLn (show state')
                      return state'
                    WS.sendTextData conn (encode CreateRoomResponse.CreateRoomResponse { CreateRoomResponse.roomId = roomId })
                  (Just "join-room", Nothing) -> do
                      WS.sendTextData conn (encode ErrorResponse.ErrorResponse { ErrorResponse.err = "Please specify a room id as 'payload' in the request" })
                  (Just "join-room", Just roomId) -> do
                      case Map.lookup roomId (rooms state) of
                        Nothing -> do
                          WS.sendTextData conn (encode ErrorResponse.ErrorResponse { ErrorResponse.err = "No room exists with id " ++ roomId })
                        Just _ -> do
                          modifyMVar_ stateM $ \state -> do
                            let state' = moveClientToRoom roomId client state
                            putStrLn $ "Client '" ++ id ++ "' joined room " ++ roomId
                            putStrLn (show state')
                            return state'
                          state <- readMVar stateM
                          broadcast roomId (TL.toStrict . T.decodeUtf8 $ encode JoinedRoom.JoinedRoom { JoinedRoom.connId = id, JoinedRoom.name = playerName client state }) state
                          WS.sendTextData conn (encode JoinRoomResponse.JoinRoomResponse { JoinRoomResponse.roomId = roomId })
                  (Just "player-name-updated", Nothing) -> do
                      WS.sendTextData conn (encode ErrorResponse.ErrorResponse { ErrorResponse.err = "No name provided." })
                  (Just "player-name-updated", Just name) -> do
                      modifyMVar_ stateM $ \state -> do
                        let state' = updatePlayerName client name state
                        putStrLn $ "Client '" ++ id ++ "' changed name to '" ++ name ++ "'"
                        putStrLn (show state')
                        return state'
                      let response = encode PlayerNameChanged.PlayerNameChanged { PlayerNameChanged.connId = id, PlayerNameChanged.name = name }
                      let roomId = getRoomId id state
                      case roomId of
                        Nothing -> do
                          WS.sendTextData conn response
                        Just roomId -> do
                          broadcast roomId (TL.toStrict . T.decodeUtf8 $ response) state
                  (Just "new-word", _) -> do
                    word <- nextWord
                    WS.sendTextData conn (encode NewWordResponse.NewWordResponse { NewWordResponse.word = word })
                  _ -> do
                    WS.sendTextData conn (encode ErrorResponse.ErrorResponse { ErrorResponse.err = "Unknown message type" })
              Nothing -> do
                WS.sendTextData conn (encode ErrorResponse.ErrorResponse { ErrorResponse.err = "Failed to parse message" })

    httpApp :: Application
    httpApp request respond = do
      respond $ case rawPathInfo request of
          "/" -> responseFile status200 [("Content-Type", "text/html")] "index.html" Nothing
          "/hello" -> responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"
          _ -> responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

readLines = fmap Prelude.lines . readFile

mkNextWord :: [String] -> IO String
mkNextWord words = do
  i <- getStdRandom (randomR (0, length words - 1))
  return $ words !! i

makeRoomId :: IO String
makeRoomId = do
  a <- getStdRandom (randomR ('a', 'z'))
  b <- getStdRandom (randomR ('a', 'z'))
  c <- getStdRandom (randomR ('a', 'z'))
  d <- getStdRandom (randomR ('a', 'z'))
  return [a,b,c,d]

main :: IO ()
main = do
  easyWords <- readLines "words-easy.txt"
  mediumWords <- readLines "words-medium.txt"
  hardWords <- readLines "words-hard.txt"
  let words = easyWords ++ mediumWords ++ hardWords
  state <- newMVar newServerState
  putStrLn $ "There are " ++ (show (length words)) ++ " words."
  putStrLn $ "http://localhost:8080"
  run 8080 (app (mkNextWord words) state)
