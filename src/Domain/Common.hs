{-# LANGUAGE OverloadedStrings #-}
module Domain.Common where

import Data.Text (Text)

class HasId a where
  getId :: a -> Int

class Person a where
  personName  :: a -> Text
  personEmail :: a -> Text
