{-# LANGUAGE OverloadedStrings #-}

module Password.Server where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_, swapMVar)
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
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Password.ServerState
import Password.WSResponse
import WaiAppStatic.Types (unsafeToPiece)

broadcast :: T.Text -> [Client] -> IO ()
broadcast message clients = do
  print "broadcasting message: "
  print message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastToRoom :: RoomId -> T.Text -> ServerState -> IO ()
broadcastToRoom roomId message s = do
  T.putStrLn $ T.pack $ "broadcast to room " ++ roomId ++ ": " ++ T.unpack message
  broadcast message (getRoomClients roomId s)

broadcastGame :: RoomId -> PasswordGame -> ServerState -> IO ()
broadcastGame roomId game s = do
  print $ "broadcast game to room " ++ roomId ++ ": game = " ++ show game
  let clueGivers = Prelude.filter
                    (\connId -> connId /= Password.ServerState.teamAGuesser game && connId /= Password.ServerState.teamBGuesser game)
                   $ Password.ServerState.teamA game ++ Password.ServerState.teamB game
  let gameUpdated = GameUpdated game
  let maskedGame = GameUpdated game { Password.ServerState.word = "" }
  let guessers = [Password.ServerState.teamAGuesser game, Password.ServerState.teamBGuesser game ]
  broadcast (TL.toStrict . T.decodeUtf8 $ encode gameUpdated) (getClients clueGivers s)
  broadcast (TL.toStrict . T.decodeUtf8 $ encode maskedGame) (getClients guessers s)

mkApp :: IO Application
mkApp = do
  initialState <- newServerState
  stateM <- newMVar initialState
  return (app stateM)

handleMessage :: MVar ServerState -> Client -> Map String String -> IO ()
handleMessage stateM client msgMap = do
  let (id, conn) = client
  case (Map.lookup "type" msgMap, Map.lookup "payload" msgMap) of
    (Just "create-room", _) -> do
      roomId <- makeRoomId
      modifyMVar_ stateM $ \state -> do
        let state' = moveClientToRoom roomId client state
        print $ "Client '" ++ id ++ "' created and joined room " ++ roomId
        print state'
        return state'
      WS.sendTextData conn (encode CreateRoomResponse { roomId = roomId })
    (Just "join-room", Nothing) ->
        WS.sendTextData conn (encode $ ErrorResponse "Please specify a room id as 'payload' in the request")
    (Just "join-room", Just roomId) -> do
        state <- readMVar stateM
        case Map.lookup roomId (rooms state) of
          Nothing ->
            WS.sendTextData conn (encode ErrorResponse { err = "No room exists with id " ++ roomId })
          Just _ -> do
            modifyMVar_ stateM $ \state -> do
              let state' = moveClientToRoom roomId client state
              print $ "Client '" ++ id ++ "' joined room " ++ roomId
              print state'
              return state'
            state <- readMVar stateM
            broadcastToRoom roomId (TL.toStrict . T.decodeUtf8 $ encode JoinedRoom { connId = id, name = playerName client state }) state
            let roomClients = getRoomClients roomId state
            let playerNamesById = Map.fromList $ Prelude.map (\client -> (fst client, playerName client state)) roomClients
            WS.sendTextData conn (encode JoinRoomResponse { roomId = roomId, playerNamesById = playerNamesById })
    (Just "player-name-updated", Nothing) ->
        WS.sendTextData conn (encode ErrorResponse { err = "No name provided." })
    (Just "player-name-updated", Just name) -> do
        modifyMVar_ stateM $ \state -> do
          let state' = updatePlayerName client name state
          print $ "Client '" ++ id ++ "' changed name to '" ++ name ++ "'"
          print state'
          return state'
        let response = encode PlayerNameChanged { connId = id, name = name }
        state <- readMVar stateM
        let roomId = getRoomId id state
        case roomId of
          Nothing ->
            WS.sendTextData conn response
          Just roomId ->
            broadcastToRoom roomId (TL.toStrict . T.decodeUtf8 $ response) state
    (Just "new-word", _) -> do
      state <- readMVar stateM
      word <- nextWord (gameWords state) easy
      WS.sendTextData conn (encode NewWordResponse { Password.WSResponse.word = word })
    (Just "start-game", _) -> do
      state <- readMVar stateM
      case getRoomId id state of
        Nothing ->
          WS.sendTextData conn (encode ErrorResponse { err = "Yikes, you asked to start a game, but you aren't in a room." })
        Just roomId ->
          if length (getRoomClients roomId state) < 4 then
            WS.sendTextData conn (encode ErrorResponse { err = "You need at least 4 people to start a game." })
          else do
              game <- newGameInRoom (gameWords state) (getRoomClients roomId state)
              updateGameState stateM roomId game
    (Just "submit-clue", Nothing) ->
        WS.sendTextData conn (encode ErrorResponse { err = "You must supply a clue as 'payload' in the message" })
    (Just "submit-clue", Just clue) -> do
        print $ "Got a clue submission: " ++ clue
        state <- readMVar stateM
        case getRoomId id state of
          Nothing ->
            WS.sendTextData conn (encode ErrorResponse { err = "idk what room you are in" })
          Just roomId -> do
            print $ "Found room " ++ roomId
            case Map.lookup roomId (games state) of
              Nothing ->
                WS.sendTextData conn (encode ErrorResponse { err = "you tried to submit a word, but there's no game in progress" })
              Just game ->
                updateGameState stateM roomId game { clues = clue : clues game }
    (Just "submit-guess", Just guess) -> do
        state <- readMVar stateM
        case getRoomId id state of
          Nothing ->
            WS.sendTextData conn (encode ErrorResponse { err = "idk what room you are in" })
          Just roomId ->
            case Map.lookup roomId (games state) of
              Nothing ->
                WS.sendTextData conn (encode ErrorResponse { err = "you tried to submit a guess, but there's no game in progress" })
              Just game ->
                if teamAGuesser game /= id && teamBGuesser game /= id
                then
                  WS.sendTextData conn (encode ErrorResponse { err = "you are not a guesser!" })
                else
                  -- TODO: is the guess correct? then setup for the next round
                  updateGameState stateM roomId game { guesses = guess : guesses game }
    _ ->
      WS.sendTextData conn (encode ErrorResponse { err = "Unknown message type" })

updateGameState :: MVar ServerState -> RoomId -> PasswordGame -> IO ()
updateGameState stateM roomId game = do
  modifyMVar_ stateM $ return . setGameInRoom roomId game
  state' <- readMVar stateM
  broadcastGame roomId game state'

app :: MVar ServerState -> Application
app stateM = websocketsOr WS.defaultConnectionOptions wsApp httpApp
  where
    disconnect client = modifyMVar_ stateM $ \s -> do
      print $ "Client with id '" ++ fst client ++ "' disconnected."
      return $ removeClient client s

    wsApp :: WS.ServerApp
    wsApp pending_conn = do
      conn <- WS.acceptRequest pending_conn
      id <- fmap UUID.toString UUID.nextRandom
      let client = (id, conn)
      WS.sendTextData conn (encode (IdentifyConnection id))
      modifyMVar_ stateM $ \state -> do
        print $ "Adding user " ++ id ++ " to lobby"
        return $ addToLobby client (addClient client state)
      flip finally (disconnect client) $
        WS.withPingThread conn 30 (return ()) $
          forever $ do
            msg <- WS.receiveData conn
            print $ "Got message from " ++ id
            case decode msg :: Maybe (Map String String) of
              Just msgMap -> handleMessage stateM client msgMap
              Nothing ->
                WS.sendTextData conn (encode ErrorResponse { err = "Failed to parse message" })

    httpApp :: Application
    httpApp = staticApp staticAppSettings

    staticAppSettings = (defaultWebAppSettings "frontend/build") { ssIndices = [unsafeToPiece "index.html"]}


