{-# LANGUAGE OverloadedStrings #-}
module Domain.Section (Section(..)) where

import Data.Text (Text)
import Domain.Common

data Section = Section
  { sectionId   :: Int
  , sectionName :: Text
  , description :: Text
  } deriving (Show, Eq)

instance HasId Section where
  getId = sectionId
