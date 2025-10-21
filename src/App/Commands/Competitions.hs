{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Competitions (competitionsP, CompetitionsCmd(..), handleCompetitions) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import UseCases.Competitions
import qualified Domain.Competition as DC

data CompetitionsCmd
  = CList | CListPretty | CListBySec { csec :: Int }
  | CAdd { ctitle :: String, csecid :: Int, cdate :: String, cloc :: Int }
  | CDel { cidDel :: Int }
  | CEdit { ceid :: Int, cetitle :: Maybe String, cesecid :: Maybe Int, cedate :: Maybe String, celoc :: Maybe Int }
  deriving (Show)

competitionsP :: Parser CompetitionsCmd
competitionsP = hsubparser $
  command "list" (info (pure CList) (progDesc "Список змагань")) <>
  command "list-pretty" (info (pure CListPretty) (progDesc "Змагання з секціями та локаціями")) <>
  command "list-by-section" (info cListBySec (progDesc "Змагання секції")) <>
  command "add" (info cAdd (progDesc "Додати змагання")) <>
  command "del" (info cDel (progDesc "Видалити змагання")) <>
  command "edit" (info cEdit (progDesc "Редагувати змагання"))
  where
    cListBySec = CListBySec <$> option auto (long "section-id")
    cAdd = CAdd
      <$> strOption (long "title") <*> option auto (long "section-id")
      <*> strOption (long "date") <*> option auto (long "location-id")
    cDel = CDel <$> option auto (long "id")
    cEdit = CEdit <$> option auto (long "id")
      <*> optional (strOption (long "title"))
      <*> optional (option auto (long "section-id"))
      <*> optional (strOption (long "date"))
      <*> optional (option auto (long "location-id"))

handleCompetitions :: DbPool -> CompetitionsCmd -> IO ()
handleCompetitions pool cmd =
  withConn pool $ \conn -> case cmd of
    CList -> do
      xs <- listCompetitions conn
      mapM_ (\c -> putStrLn $ show (DC.competitionId c) <> " | "
        <> T.unpack (DC.title c) <> " | " <> show (DC.dateDay c)
        <> " | sec " <> show (DC.sectionId c)) xs
    CListPretty -> do
      xs <- listCompetitionsPretty conn
      mapM_ (\(cid, ttl, d, sec, loc) ->
        putStrLn $ show cid <> " | " <> T.unpack ttl <> " | " <> show d
               <> " | секція: " <> T.unpack sec <> " | локація: " <> T.unpack loc) xs
    CListBySec sid -> listCompetitionsBySection conn sid >>= mapM_ print
    CAdd ttl sid d loc -> addCompetition conn (T.pack ttl) sid (read d) loc >> putStrLn "✅ Додано."
    CDel i -> deleteCompetition conn i >> putStrLn "🗑️  Видалено."
    CEdit cid mt ms md ml -> editCompetition conn cid (fmap T.pack mt) ms (fmap read md) ml >> putStrLn "✏️  Оновлено."
