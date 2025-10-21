{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Sections (sectionsP, SectionsCmd(..), handleSections) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import qualified Domain.Section as DSec
import UseCases.Sections

data SectionsCmd
  = SList
  | SAdd { sname :: String, sdescr :: String }
  | SDel { secid :: Int }
  | SEdit { seid2 :: Int, snameE :: Maybe String, sdescrE :: Maybe String }
  deriving (Show)

sectionsP :: Parser SectionsCmd
sectionsP = hsubparser $
  command "list" (info (pure SList) (progDesc "Список секцій")) <>
  command "add"  (info sAdd (progDesc "Додати секцію")) <>
  command "del"  (info sDel (progDesc "Видалити секцію")) <>
  command "edit" (info sEdit (progDesc "Редагувати секцію"))
  where
    sAdd = SAdd
      <$> strOption (long "name" <> help "Назва секції")
      <*> strOption (long "descr" <> help "Опис")
    sDel = SDel <$> option auto (long "id" <> help "ID секції")
    sEdit = SEdit
      <$> option auto (long "id" <> help "ID секції")
      <*> optional (strOption (long "name"))
      <*> optional (strOption (long "descr"))

handleSections :: DbPool -> SectionsCmd -> IO ()
handleSections pool cmd =
  withConn pool $ \conn -> case cmd of
    SList -> do
      xs <- listSections conn
      if null xs then putStrLn "Секцій немає."
      else mapM_ (\s -> putStrLn $ show (DSec.sectionId s) <> " | " <> T.unpack (DSec.sectionName s)) xs
    SAdd nm ds -> addSection conn (T.pack nm) (T.pack ds) >> putStrLn "✅ Секцію додано."
    SDel i -> deleteSection conn i >> putStrLn "🗑️  Секцію видалено."
    SEdit sid mname mdescr ->
      editSection conn sid (fmap T.pack mname) (fmap T.pack mdescr)
               >> putStrLn "✏️  Секцію оновлено."
