{-# LANGUAGE OverloadedStrings #-}
module UseCases.Coaches
  ( addCoach
  , deleteCoach
  , listCoachesBySectionPretty
  , listCoachesByTeacherPretty
  ) where

import           Database.MySQL.Simple
import           Data.Text (Text)
import           Infra.DB.Sql (q)

-- Закріпити викладача за секцією
addCoach :: Connection -> Int -> Int -> IO ()
addCoach conn teacherId sectionId = do
  _ <- execute conn
    "INSERT INTO coaches(teacher_id, section_id, assigned_at) VALUES (?,?,CURDATE())"
    (teacherId, sectionId)
  pure ()

-- Відкріпити викладача від секції
deleteCoach :: Connection -> Int -> Int -> IO ()
deleteCoach conn teacherId sectionId = do
  _ <- execute conn
        "DELETE FROM coaches WHERE teacher_id=? AND section_id=?"
        (teacherId, sectionId)
  pure ()

-- Для секції: (teacher_id, "ПІБ викладача")
listCoachesBySectionPretty :: Connection -> Int -> IO [(Int, Text)]
listCoachesBySectionPretty conn sid = do
  rows <- q conn
    "SELECT t.id, CONCAT(t.first_name,' ',t.last_name) \
    \FROM coaches c JOIN teachers t ON t.id=c.teacher_id \
    \WHERE c.section_id=? ORDER BY t.last_name, t.first_name"
    (Only sid)
  pure rows

-- Для викладача: (section_id, name)
listCoachesByTeacherPretty :: Connection -> Int -> IO [(Int, Text)]
listCoachesByTeacherPretty conn tid = do
  rows <- q conn
    "SELECT s.id, s.name \
    \FROM coaches c JOIN sections s ON s.id=c.section_id \
    \WHERE c.teacher_id=? ORDER BY s.name"
    (Only tid)
  pure rows
