{-# LANGUAGE OverloadedStrings #-}
module Domain.Competition (Competition(..)) where

import Data.Text (Text)
import Data.Time (Day)
import Domain.Common

data Competition = Competition
  { competitionId :: Int
  , title         :: Text
  , sectionId     :: Int
  , dateDay       :: Day
  , locationId    :: Int
  } deriving (Show, Eq)

instance HasId Competition where getId = competitionId
