{-# LANGUAGE OverloadedStrings #-}

module Password.Server where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, swapMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.Map.Internal as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as T
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
  putStrLn $ "broadcasting message: " ++ (show message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastToRoom :: RoomId -> T.Text -> ServerState -> IO ()
broadcastToRoom aRoomId message s = do
  T.putStrLn $ T.pack $ "broadcast to room " ++ aRoomId ++ ": " ++ T.unpack message
  broadcast message (getRoomClients aRoomId s)

broadcastGame :: RoomId -> PasswordGame -> ServerState -> IO ()
broadcastGame aRoomId game s = do
  putStrLn $ "broadcast game to room " ++ aRoomId ++ ": game = " ++ show game
  let clueGivers =
        Prelude.filter
          (\aConnId -> aConnId /= Password.ServerState.teamAGuesser game && aConnId /= Password.ServerState.teamBGuesser game)
          $ Password.ServerState.teamA game ++ Password.ServerState.teamB game
  let gameUpdated = GameUpdated game
  let maskedGame = GameUpdated game {Password.ServerState.word = ""}
  let guessers = [Password.ServerState.teamAGuesser game, Password.ServerState.teamBGuesser game]
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
      aRoomId <- makeRoomId
      modifyMVar_ stateM $ \state -> do
        let state' = moveClientToRoom aRoomId client state
        putStrLn $ "Client '" ++ id ++ "' created and joined room " ++ aRoomId
        print state'
        return state'
      WS.sendTextData conn (encode CreateRoomResponse {roomId = aRoomId})
    (Just "join-room", Nothing) ->
      WS.sendTextData conn (encode $ ErrorResponse "Please specify a room id as 'payload' in the request")
    (Just "join-room", Just aRoomId) -> do
      state <- readMVar stateM
      case Map.lookup aRoomId (rooms state) of
        Nothing ->
          WS.sendTextData conn (encode ErrorResponse {err = "No room exists with id " ++ aRoomId})
        Just _ -> do
          modifyMVar_ stateM $ \state_ -> do
            let state' = moveClientToRoom aRoomId client state_
            putStrLn $ "Client '" ++ id ++ "' joined room " ++ aRoomId
            print state'
            return state'
          state_ <- readMVar stateM
          broadcastToRoom aRoomId (TL.toStrict . T.decodeUtf8 $ encode JoinedRoom {connId = id, name = playerName client state_}) state_
          let roomClients = getRoomClients aRoomId state_
          WS.sendTextData
            conn
            ( encode
                JoinRoomResponse
                  { roomId = aRoomId,
                    playerNamesById = Map.fromList $ Prelude.map (\client_ -> (fst client_, playerName client_ state_)) roomClients
                  }
            )
    (Just "player-name-updated", Nothing) ->
      WS.sendTextData conn (encode ErrorResponse {err = "No name provided."})
    (Just "player-name-updated", Just name) -> do
      modifyMVar_ stateM $ \state -> do
        let state' = updatePlayerName client name state
        putStrLn $ "Client '" ++ id ++ "' changed name to '" ++ name ++ "'"
        print state'
        return state'
      let response = encode PlayerNameChanged {connId = id, name = name}
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
      WS.sendTextData conn (encode NewWordResponse {Password.WSResponse.word = word})
    (Just "start-game", _) -> do
      state <- readMVar stateM
      case getRoomId id state of
        Nothing ->
          WS.sendTextData conn (encode ErrorResponse {err = "Yikes, you asked to start a game, but you aren't in a room."})
        Just roomId ->
          if length (getRoomClients roomId state) < 4
            then WS.sendTextData conn (encode ErrorResponse {err = "You need at least 4 people to start a game."})
            else do
              game <- newGameInRoom (gameWords state) (getRoomClients roomId state)
              updateGameState stateM roomId game
    (Just "submit-clue", Nothing) ->
      WS.sendTextData conn (encode ErrorResponse {err = "You must supply a clue as 'payload' in the message"})
    (Just "submit-clue", Just clue) -> do
      putStrLn $ "Got a clue submission: " ++ clue
      state <- readMVar stateM
      case getRoomId id state of
        Nothing ->
          WS.sendTextData conn (encode ErrorResponse {err = "idk what room you are in"})
        Just roomId -> do
          putStrLn $ "Found room " ++ roomId
          case Map.lookup roomId (games state) of
            Nothing ->
              WS.sendTextData conn (encode ErrorResponse {err = "you tried to submit a word, but there's no game in progress"})
            Just game -> do
              result <- submitClue id clue (gameWords state) game
              case result of
                Left err -> WS.sendTextData conn (encode ErrorResponse {err = err})
                Right game' -> updateGameState stateM roomId game'
    (Just "submit-guess", Just guess) -> do
      state <- readMVar stateM
      case getRoomId id state of
        Nothing ->
          WS.sendTextData conn (encode ErrorResponse {err = "idk what room you are in"})
        Just roomId ->
          case Map.lookup roomId (games state) of
            Nothing ->
              WS.sendTextData conn (encode ErrorResponse {err = "you tried to submit a guess, but there's no game in progress"})
            Just game -> do
              result <- guessWord id guess (gameWords state) game
              case result of
                Left err -> WS.sendTextData conn (encode ErrorResponse {err = err})
                Right game' -> updateGameState stateM roomId game'
    _ ->
      WS.sendTextData conn (encode ErrorResponse {err = "Unknown message type"})

updateGameState :: MVar ServerState -> RoomId -> PasswordGame -> IO ()
updateGameState stateM aRoomId game = do
  modifyMVar_ stateM $ return . setGameInRoom aRoomId game
  state' <- readMVar stateM
  broadcastGame aRoomId game state'

app :: MVar ServerState -> Application
app stateM = websocketsOr WS.defaultConnectionOptions wsApp httpApp
  where
    disconnect client = modifyMVar_ stateM $ \s -> do
      putStrLn $ "Client with id '" ++ fst client ++ "' disconnected."
      return $ removeClient client s

    wsApp :: WS.ServerApp
    wsApp pending_conn = do
      conn <- WS.acceptRequest pending_conn
      id <- fmap UUID.toString UUID.nextRandom
      let client = (id, conn)
      WS.sendTextData conn (encode (IdentifyConnection id))
      modifyMVar_ stateM $ \state -> do
        putStrLn $ "Adding user " ++ id ++ " to lobby"
        return $ addToLobby client (addClient client state)
      flip finally (disconnect client) $
        WS.withPingThread conn 30 (return ()) $
          forever $ do
            msg <- WS.receiveData conn
            putStrLn $ "Got message from " ++ id
            case decode msg :: Maybe (Map String String) of
              Just msgMap -> handleMessage stateM client msgMap
              Nothing ->
                WS.sendTextData conn (encode ErrorResponse {err = "Failed to parse message"})

    httpApp :: Application
    httpApp = staticApp staticAppSettings

    staticAppSettings = (defaultWebAppSettings "frontend/build") {ssIndices = [unsafeToPiece "index.html"]}
