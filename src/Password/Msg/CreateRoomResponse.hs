{-# LANGUAGE DeriveGeneric #-}

module Password.Msg.CreateRoomResponse where

import Data.Aeson
import GHC.Generics

data CreateRoomResponse  = CreateRoomResponse { roomId :: String } deriving (Generic, Show)
instance ToJSON CreateRoomResponse
