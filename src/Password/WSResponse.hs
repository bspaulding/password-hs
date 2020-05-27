{-# LANGUAGE DeriveGeneric #-}

module Password.WSResponse where

import Data.Aeson
import Data.Map.Internal as Map
import GHC.Generics
import Password.ServerState

data WSResponse
  = ErrorResponse { err :: String }
  | IdentifyConnection { connId :: String }
  | CreateRoomResponse { roomId :: String }
  | JoinedRoom { connId :: String, name :: String }
  | JoinRoomResponse { roomId :: String, playerNamesById :: Map ConnId String }
  | NewWordResponse { word :: String }
  | PlayerNameChanged { connId :: String, name :: String }
  | GameUpdated PasswordGame
  deriving (Generic, Show)

wsResponseToJSONOptions = defaultOptions { sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = "payload" } }
instance ToJSON WSResponse where
  toJSON = genericToJSON wsResponseToJSONOptions
  toEncoding = genericToEncoding wsResponseToJSONOptions

