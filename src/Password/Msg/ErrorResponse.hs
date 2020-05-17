{-# LANGUAGE DeriveGeneric #-}

module Password.Msg.ErrorResponse where

import Data.Aeson
import GHC.Generics

data ErrorResponse = ErrorResponse { err :: String } deriving (Generic, Show)
instance ToJSON ErrorResponse

