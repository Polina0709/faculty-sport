{-# LANGUAGE OverloadedStrings #-}
module UseCases.Schedule
  ( listScheduleForSection
  , listSchedulePretty
  , addSchedule
  , editScheduleTime
  , deleteSchedule
  ) where

import           Database.MySQL.Simple
import qualified Data.Text as T
import           Infra.DB.Sql (q, toSchedule)
import           Domain.Schedule

-- Технічний список (получаємо start/end як текст)
listScheduleForSection :: Connection -> Int -> IO [Schedule]
listScheduleForSection conn secId = do
  let sql = "SELECT id, section_id, weekday, DATE_FORMAT(start_time,'%H:%i:%s') AS start_txt, DATE_FORMAT(end_time,'%H:%i:%s') AS end_txt, location_id FROM schedule WHERE section_id=? ORDER BY weekday, start_time"
  rows <- q conn sql (Only secId)
  pure (fmap toSchedule rows)

-- (id слоту, weekday, start, end, location_name)
listSchedulePretty :: Connection -> Int -> IO [(Int, Int, T.Text, T.Text, T.Text)]
listSchedulePretty conn secId = do
  let sql = "SELECT s.id, s.weekday, DATE_FORMAT(s.start_time,'%H:%i:%s') AS start_txt, DATE_FORMAT(s.end_time,'%H:%i:%s') AS end_txt, l.name AS location_name FROM schedule s JOIN locations l ON l.id = s.location_id WHERE s.section_id=? ORDER BY s.weekday, s.start_time"
  q conn sql (Only secId)

addSchedule :: Connection -> Int -> Int -> String -> String -> Int -> IO ()
addSchedule conn secId wd st et locId = do
  _ <- execute conn "INSERT INTO schedule(section_id, weekday, start_time, end_time, location_id) VALUES (?,?,?,?,?)"
        (secId, wd, st, et, locId)
  pure ()

editScheduleTime :: Connection -> Int -> String -> String -> IO ()
editScheduleTime conn schedId newSt newEt = do
  _ <- execute conn "UPDATE schedule SET start_time=?, end_time=? WHERE id=?"
        (newSt, newEt, schedId)
  pure ()

deleteSchedule :: Connection -> Int -> IO ()
deleteSchedule conn schedId = do
  _ <- execute conn "DELETE FROM schedule WHERE id=?" (Only schedId)
  pure ()
