{-# LANGUAGE OverloadedStrings #-}
module Domain.Location where

import Data.Text (Text)
import Domain.Common

data Location = Location
  { locationId :: Int
  , locationName    :: Text
  , address    :: Text
  } deriving (Show, Eq)

instance HasId Location where getId = locationId
