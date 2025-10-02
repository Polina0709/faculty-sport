{-# LANGUAGE OverloadedStrings #-}
module Domain.Student (Student(..)) where

import Data.Text (Text)
import Data.Time (Day)
import Domain.Common

data Student = Student
  { studentId  :: Int
  , firstName  :: Text
  , lastName   :: Text
  , groupCode  :: Text
  , birthDate  :: Day
  , email      :: Text
  } deriving (Show, Eq)

instance HasId Student where
  getId = studentId

instance Person Student where
  personName s = firstName s <> " " <> lastName s
  personEmail  = email
