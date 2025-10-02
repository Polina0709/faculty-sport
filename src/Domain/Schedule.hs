{-# LANGUAGE OverloadedStrings #-}
module Domain.Schedule (Schedule(..)) where

import Domain.Common

data Schedule = Schedule
  { scheduleId :: Int
  , sectionId  :: Int
  , weekday    :: Int     -- 1..7
  , startTime  :: String  -- "HH:MM:SS"
  , endTime    :: String
  , locationId :: Int
  } deriving (Show, Eq)

instance HasId Schedule where getId = scheduleId
