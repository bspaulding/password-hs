{-# LANGUAGE DeriveGeneric #-}

module Password.Msg.PlayerNameChanged where

import Data.Aeson
import GHC.Generics

data PlayerNameChanged =
  PlayerNameChanged
    { connId :: String
    , name :: String
    } deriving (Generic, Show)

instance ToJSON PlayerNameChanged
