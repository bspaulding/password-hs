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
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.Random
import WaiAppStatic.Types (unsafeToPiece)

type RoomId = String
type ConnId = String
type Client = (ConnId, WS.Connection)

data WSResponse
  = ErrorResponse { err :: String }
  | IdentifyConnection { connId :: String }
  | CreateRoomResponse { roomId :: String }
  | JoinedRoom { connId :: String, name :: String }
  | JoinRoomResponse { roomId :: String }
  | NewWordResponse { word :: String }
  | PlayerNameChanged { connId :: String, name :: String }
  deriving (Generic, Show)

wsResponseToJSONOptions = defaultOptions { sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = "contents" } }
instance ToJSON WSResponse where
  toJSON = genericToJSON wsResponseToJSONOptions
  toEncoding = genericToEncoding wsResponseToJSONOptions

instance Show WS.Connection where
  show conn = "<wsconn>"

data GameWords = GameWords { easy :: [String], medium :: [String], hard :: [String] } deriving (Show)

data ServerState =
  ServerState
    { rooms :: Map RoomId [Client]
    , roomIdByConnId :: Map ConnId RoomId
    , lobby :: [Client]
    , names :: Map ConnId String
    , gameWords :: GameWords
    } deriving (Show)

newServerState :: GameWords -> ServerState
newServerState gameWords =
  ServerState
    { rooms = Map.empty
    , lobby = []
    , names = Map.empty
    , roomIdByConnId = Map.empty
    , gameWords = gameWords
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

app :: MVar ServerState -> Application
app stateM = websocketsOr WS.defaultConnectionOptions wsApp httpApp
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
      WS.sendTextData conn (encode (IdentifyConnection id))
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
                    WS.sendTextData conn (encode CreateRoomResponse { roomId = roomId })
                  (Just "join-room", Nothing) -> do
                      WS.sendTextData conn (encode $ ErrorResponse "Please specify a room id as 'payload' in the request")
                  (Just "join-room", Just roomId) -> do
                      case Map.lookup roomId (rooms state) of
                        Nothing -> do
                          WS.sendTextData conn (encode ErrorResponse { err = "No room exists with id " ++ roomId })
                        Just _ -> do
                          modifyMVar_ stateM $ \state -> do
                            let state' = moveClientToRoom roomId client state
                            putStrLn $ "Client '" ++ id ++ "' joined room " ++ roomId
                            putStrLn (show state')
                            return state'
                          state <- readMVar stateM
                          broadcast roomId (TL.toStrict . T.decodeUtf8 $ encode JoinedRoom { connId = id, name = playerName client state }) state
                          -- TODO: Send current room state, including word and players in room
                          WS.sendTextData conn (encode JoinRoomResponse { roomId = roomId })
                  (Just "player-name-updated", Nothing) -> do
                      WS.sendTextData conn (encode ErrorResponse { err = "No name provided." })
                  (Just "player-name-updated", Just name) -> do
                      modifyMVar_ stateM $ \state -> do
                        let state' = updatePlayerName client name state
                        putStrLn $ "Client '" ++ id ++ "' changed name to '" ++ name ++ "'"
                        putStrLn (show state')
                        return state'
                      let response = encode PlayerNameChanged { connId = id, name = name }
                      let roomId = getRoomId id state
                      case roomId of
                        Nothing -> do
                          WS.sendTextData conn response
                        Just roomId -> do
                          broadcast roomId (TL.toStrict . T.decodeUtf8 $ response) state
                  (Just "new-word", _) -> do
                    word <- nextWord (gameWords state) easy
                    WS.sendTextData conn (encode NewWordResponse { word = word })
                  _ -> do
                    WS.sendTextData conn (encode ErrorResponse { err = "Unknown message type" })
              Nothing -> do
                WS.sendTextData conn (encode ErrorResponse { err = "Failed to parse message" })

    httpApp :: Application
    httpApp = staticApp $ (defaultWebAppSettings "frontend/build") { ssIndices = [unsafeToPiece "index.html"]}

readLines = fmap Prelude.lines . readFile

mkNextWord :: [String] -> IO String
mkNextWord words = do
  i <- getStdRandom (randomR (0, length words - 1))
  return $ words !! i

nextWord :: GameWords -> (GameWords -> [String]) -> IO String
nextWord gameWords cat = mkNextWord $ cat gameWords

loadGameWords :: IO GameWords
loadGameWords = do
  easyWords <- readLines "words-easy.txt"
  mediumWords <- readLines "words-medium.txt"
  hardWords <- readLines "words-hard.txt"
  return $ GameWords { easy = easyWords , medium = mediumWords , hard = hardWords }

makeRoomId :: IO String
makeRoomId = do
  a <- getStdRandom chars
  b <- getStdRandom chars
  c <- getStdRandom chars
  d <- getStdRandom chars
  return [a,b,c,d]
  where chars = randomR ('a', 'z')

main :: IO ()
main = do
  gameWords <- loadGameWords
  state <- newMVar (newServerState gameWords)
  print "http://localhost:8080"
  run 8080 (app state)
