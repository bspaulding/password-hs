{-# LANGUAGE DeriveGeneric #-}

module Password.Msg.JoinRoomResponse where

import Data.Aeson
import GHC.Generics

data JoinRoomResponse = JoinRoomResponse { roomId :: String } deriving (Generic, Show)
instance ToJSON JoinRoomResponse
