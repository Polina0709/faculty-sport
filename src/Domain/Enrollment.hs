{-# LANGUAGE OverloadedStrings #-}
module Domain.Enrollment where

import Data.Time (Day)
import Domain.Common

data Enrollment = Enrollment
  { enrollmentId :: Int
  , studentId    :: Int
  , sectionId    :: Int
  , enrolledAt   :: Day
  } deriving (Show, Eq)

instance HasId Enrollment where getId = enrollmentId

