{-# LANGUAGE DeriveGeneric #-}

module Password.WSResponse where

import Data.Aeson
import GHC.Generics

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

