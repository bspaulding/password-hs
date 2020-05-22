module Password.ServerState where

import Data.Map.Internal as Map
import qualified Network.WebSockets as WS
import System.Random

type RoomId = String
type ConnId = String
type Client = (ConnId, WS.Connection)

data GameWords = GameWords { easy :: [String], medium :: [String], hard :: [String] } deriving (Show)

instance Show WS.Connection where
  show conn = "<wsconn>"

data ServerState =
  ServerState
    { rooms :: Map RoomId [Client]
    , roomIdByConnId :: Map ConnId RoomId
    , lobby :: [Client]
    , names :: Map ConnId String
    , gameWords :: GameWords
    } deriving (Show)

newServerState :: IO ServerState
newServerState = do
  gameWords <- loadGameWords
  return ServerState
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
removeFromRooms client s = s { rooms = Map.map (Prelude.filter ((/= fst client) . fst)) (rooms s)
                             , roomIdByConnId = Map.delete (fst client) (roomIdByConnId s)
                             }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = removeFromLobby client (removeFromRooms client s)

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

mkNextWord :: [String] -> IO String
mkNextWord words = do
  i <- getStdRandom (randomR (0, length words - 1))
  return $ words !! i

nextWord :: GameWords -> (GameWords -> [String]) -> IO String
nextWord gameWords cat = mkNextWord $ cat gameWords

readLines = fmap Prelude.lines . readFile

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


