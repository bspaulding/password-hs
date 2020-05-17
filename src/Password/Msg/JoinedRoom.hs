{-# LANGUAGE DeriveGeneric #-}

module Password.Msg.JoinedRoom where

import Data.Aeson
import GHC.Generics

data JoinedRoom =
  JoinedRoom
    { connId :: String
    , name :: String
    } deriving (Generic, Show)

instance ToJSON JoinedRoom
