{-# LANGUAGE OverloadedStrings #-}
module UseCases.Locations
  ( listLocations
  , addLocation
  , deleteLocation
  , editLocation
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Database.MySQL.Simple
import           Infra.DB.Sql (q)
import           Domain.Location

-- Мапимо напряму у Domain.Location через звичайний кортеж
toLocation :: (Int, Text, Text) -> Location
toLocation (lid, nm, addr) = Location { locationId = lid, locationName = nm, address = addr }

listLocations :: Connection -> IO [Location]
listLocations conn = do
  rows <- q conn
    "SELECT id, name, address FROM locations ORDER BY name" ()
  pure (map toLocation rows)

addLocation :: Connection -> Text -> Text -> IO ()
addLocation conn nm addr = do
  _ <- execute conn
    "INSERT INTO locations(name, address) VALUES (?,?)"
    (nm, addr)
  pure ()

deleteLocation :: Connection -> Int -> IO ()
deleteLocation conn lid = do
  _ <- execute conn
    "DELETE FROM locations WHERE id=?"
    (Only lid)
  pure ()

editLocation :: Connection -> Int -> Maybe Text -> Maybe Text -> IO ()
editLocation conn lid mname maddr = do
  let sql = "UPDATE locations \
            \SET name = COALESCE(?, name), \
            \    address = COALESCE(?, address) \
            \WHERE id=?"
  _ <- execute conn sql (mname, maddr, lid)
  pure ()