{-# LANGUAGE DeriveGeneric #-}

module Password.ServerState where

import Data.Aeson
import Data.Map.Internal as Map
import Data.Maybe
import GHC.Generics
import qualified Network.WebSockets as WS
import System.Random

type RoomId = String

type ConnId = String

type Client = (ConnId, WS.Connection)

data Possession = TeamA | TeamB deriving (Generic, Show, Eq)

instance ToJSON Possession

-- TODO: we can compute scores if we track rounds,
--       a round is the word, clues, guesses, clue givers, and guessers
data PasswordGame = PasswordGame
  { teamA :: [ConnId],
    teamB :: [ConnId],
    teamAClueGiver :: ConnId,
    teamBClueGiver :: ConnId,
    teamAGuesser :: ConnId,
    teamBGuesser :: ConnId,
    word :: String,
    clues :: [String],
    guesses :: [String],
    teamAScore :: Int,
    teamBScore :: Int,
    possession :: Possession
  }
  deriving (Generic, Show, Eq)

instance ToJSON PasswordGame

splitInTwo :: [a] -> ([a], [a])
splitInTwo xs =
  (Prelude.take n xs, Prelude.take n (Prelude.drop n xs))
  where
    l = length xs
    hl = div l 2
    n = if mod l 2 == 1 then hl + 1 else hl

newGameInRoom :: GameWords -> [Client] -> IO PasswordGame
newGameInRoom gameWords clients = do
  let (teamA, teamB) = splitInTwo (Prelude.map fst clients)
  teamAGuesser <- takeRand teamA
  teamBGuesser <- takeRand teamB
  teamAClueGiver <- takeRand (Prelude.filter (/= teamAGuesser) teamA)
  teamBClueGiver <- takeRand (Prelude.filter (/= teamAGuesser) teamB)
  word <- nextWord gameWords easy
  return
    PasswordGame
      { teamA = teamA,
        teamB = teamB,
        teamAGuesser = teamAGuesser,
        teamBGuesser = teamBGuesser,
        teamAClueGiver = teamAClueGiver,
        teamBClueGiver = teamBClueGiver,
        word = word,
        clues = [],
        guesses = [],
        teamAScore = 0,
        teamBScore = 0,
        possession = TeamA
      }

nextRoundInGame :: GameWords -> PasswordGame -> IO PasswordGame
nextRoundInGame gameWords game = do
  teamAGuesser <- takeRand $ Prelude.filter (/= teamAGuesser game) $ teamA game
  teamBGuesser <- takeRand $ Prelude.filter (/= teamBGuesser game) $ teamB game
  teamAClueGiver <- takeRand (Prelude.filter (/= teamAGuesser) (teamA game))
  teamBClueGiver <- takeRand (Prelude.filter (/= teamBGuesser) (teamB game))
  word <- nextWord gameWords easy
  return
    game
      { teamAGuesser = teamAGuesser,
        teamBGuesser = teamBGuesser,
        teamAClueGiver = teamAClueGiver,
        teamBClueGiver = teamBClueGiver,
        word = word,
        guesses = [],
        clues = [],
        possession = case possession game of
          TeamA -> TeamB
          TeamB -> TeamA
      }

guessWord :: ConnId -> String -> GameWords -> PasswordGame -> IO (Either String PasswordGame)
guessWord id guess gameWords game =
  case (teamAGuesser game == id, teamBGuesser game == id) of
    (False, False) -> return $ Left "You are not a guesser!"
    _ ->
      if length (guesses game) == length (clues game)
        then return $ Left "We aren't guessing yet, wait for a clue!"
        else
          if guess == word game
            then do
              let score = 11 - length (clues game)
              let team =
                    if mod (length (clues game)) 2 /= 0
                      then possession game
                      else case possession game of
                        TeamA -> TeamB
                        TeamB -> TeamA
              game' <-
                nextRoundInGame
                  gameWords
                  game
                    { guesses = guess : guesses game,
                      teamAScore = case team of
                        TeamA -> score + teamAScore game
                        TeamB -> teamAScore game,
                      teamBScore = case team of
                        TeamA -> teamBScore game
                        TeamB -> score + teamBScore game
                    }
              return $ Right game'
            else return $ Right game {guesses = guess : guesses game}

data GameWords = GameWords {easy :: [String], medium :: [String], hard :: [String]} deriving (Show)

instance Show WS.Connection where
  show conn = "<wsconn>"

data ServerState = ServerState
  { clientsById :: Map ConnId Client,
    rooms :: Map RoomId [ConnId],
    roomIdByConnId :: Map ConnId RoomId,
    lobby :: [ConnId],
    names :: Map ConnId String,
    gameWords :: GameWords,
    games :: Map RoomId PasswordGame
  }
  deriving (Show)

newServerState :: IO ServerState
newServerState = do
  gameWords <- loadGameWords
  return
    ServerState
      { clientsById = Map.empty,
        rooms = Map.empty,
        lobby = [],
        names = Map.empty,
        roomIdByConnId = Map.empty,
        gameWords = gameWords,
        games = Map.empty
      }

addClient :: Client -> ServerState -> ServerState
addClient client s = s {clientsById = Map.insert (fst client) client (clientsById s)}

addToLobby :: Client -> ServerState -> ServerState
addToLobby client s = s {lobby = fst client : lobby s}

removeFromLobby :: Client -> ServerState -> ServerState
removeFromLobby client s = s {lobby = Prelude.filter (/= fst client) (lobby s)}

removeFromRooms :: Client -> ServerState -> ServerState
removeFromRooms client s =
  s
    { rooms = Map.map (Prelude.filter (/= fst client)) (rooms s),
      roomIdByConnId = Map.delete (fst client) (roomIdByConnId s)
    }

removeClient :: Client -> ServerState -> ServerState
removeClient client s =
  removeFromLobby
    client
    (removeFromRooms client s)
      { clientsById = Map.delete (fst client) (clientsById s)
      }

addToRoom :: RoomId -> Client -> ServerState -> ServerState
addToRoom roomId client s =
  s
    { rooms = insertWith (++) roomId [fst client] (rooms s),
      roomIdByConnId = insert (fst client) roomId (roomIdByConnId s)
    }

moveClientToRoom :: RoomId -> Client -> ServerState -> ServerState
moveClientToRoom roomId client s = addToRoom roomId client (removeFromLobby client s)

getRoomId :: ConnId -> ServerState -> Maybe RoomId
getRoomId connId state = Map.lookup connId (roomIdByConnId state)

getClients :: [ConnId] -> ServerState -> [Client]
getClients connIds s = Data.Maybe.mapMaybe (\connId -> Map.lookup connId (clientsById s)) connIds

getRoomClients :: RoomId -> ServerState -> [Client]
getRoomClients roomId s = getClients connIds s
  where
    connIds = Map.findWithDefault [] roomId (rooms s)

setGameInRoom :: RoomId -> PasswordGame -> ServerState -> ServerState
setGameInRoom roomId game s = s {games = Map.insert roomId game (games s)}

updatePlayerName :: Client -> String -> ServerState -> ServerState
updatePlayerName (connId, _) name s = s {names = Map.insert connId name (names s)}

playerName :: Client -> ServerState -> String
playerName (connId, _) s = Map.findWithDefault "Unknown" connId (names s)

takeRand :: [a] -> IO a
takeRand xs = do
  i <- getStdRandom (randomR (0, length xs - 1))
  return $ xs !! i

nextWord :: GameWords -> (GameWords -> [String]) -> IO String
nextWord gameWords cat = takeRand $ cat gameWords

readLines = fmap Prelude.lines . readFile

onlySingleWords :: [String] -> [String]
onlySingleWords = Prelude.filter (all ((/=) ' '))

loadGameWords :: IO GameWords
loadGameWords = do
  easyWords <- onlySingleWords <$> readLines "words-easy.txt"
  mediumWords <- onlySingleWords <$> readLines "words-medium.txt"
  hardWords <- onlySingleWords <$> readLines "words-hard.txt"
  return $ GameWords {easy = easyWords, medium = mediumWords, hard = hardWords}

makeRoomId :: IO String
makeRoomId = do
  a <- getStdRandom chars
  b <- getStdRandom chars
  c <- getStdRandom chars
  d <- getStdRandom chars
  return [a, b, c, d]
  where
    chars = randomR ('a', 'z')
