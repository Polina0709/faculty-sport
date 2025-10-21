{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Locations (locationsP, LocationsCmd(..), handleLocations) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import qualified Domain.Location as DL
import UseCases.Locations

data LocationsCmd
  = LList
  | LAdd { lname :: String, laddr :: String }
  | LDel { lidDel :: Int }
  | LEdit { leid :: Int, lnameE :: Maybe String, laddrE :: Maybe String }
  deriving (Show)

locationsP :: Parser LocationsCmd
locationsP = hsubparser $
  command "list" (info (pure LList) (progDesc "Список локацій")) <>
  command "add"  (info lAdd (progDesc "Додати локацію")) <>
  command "del"  (info lDel (progDesc "Видалити локацію")) <>
  command "edit" (info lEdit (progDesc "Редагувати локацію"))
  where
    lAdd = LAdd <$> strOption (long "name") <*> strOption (long "addr")
    lDel = LDel <$> option auto (long "id")
    lEdit = LEdit <$> option auto (long "id")
                  <*> optional (strOption (long "name"))
                  <*> optional (strOption (long "addr"))

handleLocations :: DbPool -> LocationsCmd -> IO ()
handleLocations pool cmd =
  withConn pool $ \conn -> case cmd of
    LList -> listLocations conn >>= mapM_ (\l ->
      putStrLn $ show (DL.locationId l) <> " | " <> T.unpack (DL.locationName l) <> " | " <> T.unpack (DL.address l))
    LAdd nm addr -> addLocation conn (T.pack nm) (T.pack addr) >> putStrLn "✅ Локацію додано."
    LDel i -> deleteLocation conn i >> putStrLn "🗑️  Видалено."
    LEdit lid mname maddr -> editLocation conn lid (fmap T.pack mname) (fmap T.pack maddr) >> putStrLn "✏️  Оновлено."
