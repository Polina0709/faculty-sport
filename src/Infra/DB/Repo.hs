{-# LANGUAGE OverloadedStrings #-}
module Infra.DB.Repo
  ( Repo(..)
  ) where

import Database.MySQL.Simple (Connection)

class Repo a where
  create   :: Connection -> a   -> IO a
  findById :: Connection -> Int -> IO (Maybe a)
  update   :: Connection -> a   -> IO ()
  delete   :: Connection -> a   -> IO ()
  listAll  :: Connection ->      IO [a]
