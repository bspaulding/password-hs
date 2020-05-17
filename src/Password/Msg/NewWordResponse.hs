{-# LANGUAGE DeriveGeneric #-}

module Password.Msg.NewWordResponse where

import Data.Aeson
import GHC.Generics

data NewWordResponse = NewWordResponse { word :: String } deriving (Generic, Show)
instance ToJSON NewWordResponse
