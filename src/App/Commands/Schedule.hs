{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Schedule (scheduleP, ScheduleCmd(..), handleSchedule) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import UseCases.Schedule

data ScheduleCmd
  = SchList { secIdForList :: Int }
  | SchAdd  { secIdA :: Int, wdA :: Int, stA :: String, etA :: String, locA :: Int }
  | SchEdit { schIdE :: Int, stE :: String, etE :: String }
  | SchDel  { schIdD :: Int }
  deriving (Show)

scheduleP :: Parser ScheduleCmd
scheduleP = hsubparser $
  command "list" (info schList (progDesc "Список розкладу секції")) <>
  command "add"  (info schAdd  (progDesc "Додати слот")) <>
  command "edit" (info schEdit (progDesc "Редагувати час")) <>
  command "del"  (info schDel  (progDesc "Видалити слот"))
  where
    schList = SchList <$> option auto (long "section-id" <> help "ID секції")
    schAdd  = SchAdd
      <$> option auto (long "section-id")
      <*> option auto (long "weekday")
      <*> strOption (long "start")
      <*> strOption (long "end")
      <*> option auto (long "location-id")
    schEdit = SchEdit
      <$> option auto (long "id")
      <*> strOption (long "start")
      <*> strOption (long "end")
    schDel = SchDel <$> option auto (long "id")

handleSchedule :: DbPool -> ScheduleCmd -> IO ()
handleSchedule pool cmd =
  withConn pool $ \conn -> case cmd of
    SchList sid -> do
      xs <- listSchedulePretty conn sid
      if null xs then putStrLn "Розкладу немає."
      else mapM_ (\(slotId, wd, st, et, locName) -> putStrLn $
           show slotId <> " | day " <> show wd <> " | " <> T.unpack st <> "-" <> T.unpack et
           <> " | " <> T.unpack locName) xs
    SchAdd sid wd st et loc -> addSchedule conn sid wd st et loc >> putStrLn "✅ Слот додано."
    SchEdit sid st et -> editScheduleTime conn sid st et >> putStrLn "✏️  Оновлено."
    SchDel sid -> deleteSchedule conn sid >> putStrLn "🗑️  Видалено."
