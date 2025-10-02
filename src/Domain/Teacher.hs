{-# LANGUAGE OverloadedStrings #-}
module Domain.Teacher (Teacher(..)) where

import Data.Text (Text)
import Domain.Common

data Teacher = Teacher
  { teacherId :: Int
  , tFirst    :: Text
  , tLast     :: Text
  , position  :: Text
  , tEmail    :: Text
  } deriving (Show, Eq)

instance HasId Teacher where
  getId = teacherId

instance Person Teacher where
  personName t = tFirst t <> " " <> tLast t
  personEmail  = tEmail
