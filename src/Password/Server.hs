{-# LANGUAGE OverloadedStrings #-}

module Password.Server where

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
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Password.ServerState
import Password.WSResponse
import WaiAppStatic.Types (unsafeToPiece)

broadcast :: RoomId -> T.Text -> ServerState -> IO ()
broadcast roomId message s = do
  T.putStrLn $ T.pack $ "broadcast to room " ++ roomId ++ ": " ++ T.unpack message
  forM_ (findWithDefault [] roomId (rooms s)) $ \(_, conn) -> WS.sendTextData conn message

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
        return $ addToLobby client state
      flip finally (disconnect client) $
        WS.withPingThread conn 30 (return ()) $
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
                      print $ "Client '" ++ id ++ "' created and joined room " ++ roomId
                      print state'
                      return state'
                    WS.sendTextData conn (encode CreateRoomResponse { roomId = roomId })
                  (Just "join-room", Nothing) ->
                      WS.sendTextData conn (encode $ ErrorResponse "Please specify a room id as 'payload' in the request")
                  (Just "join-room", Just roomId) ->
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
                          broadcast roomId (TL.toStrict . T.decodeUtf8 $ encode JoinedRoom { connId = id, name = playerName client state }) state
                          -- TODO: Send current room state, including word and players in room
                          WS.sendTextData conn (encode JoinRoomResponse { roomId = roomId })
                  (Just "player-name-updated", Nothing) ->
                      WS.sendTextData conn (encode ErrorResponse { err = "No name provided." })
                  (Just "player-name-updated", Just name) -> do
                      modifyMVar_ stateM $ \state -> do
                        let state' = updatePlayerName client name state
                        print $ "Client '" ++ id ++ "' changed name to '" ++ name ++ "'"
                        print state'
                        return state'
                      let response = encode PlayerNameChanged { connId = id, name = name }
                      let roomId = getRoomId id state
                      case roomId of
                        Nothing ->
                          WS.sendTextData conn response
                        Just roomId ->
                          broadcast roomId (TL.toStrict . T.decodeUtf8 $ response) state
                  (Just "new-word", _) -> do
                    word <- nextWord (gameWords state) easy
                    WS.sendTextData conn (encode NewWordResponse { word = word })
                  _ ->
                    WS.sendTextData conn (encode ErrorResponse { err = "Unknown message type" })
              Nothing ->
                WS.sendTextData conn (encode ErrorResponse { err = "Failed to parse message" })

    httpApp :: Application
    httpApp = staticApp staticAppSettings

    staticAppSettings = (defaultWebAppSettings "frontend/build") { ssIndices = [unsafeToPiece "index.html"]}


