{-# LANGUAGE OverloadedStrings #-}
module UseCases.Enrollments
  ( enrollStudent
  , unenrollStudent
  , listEnrollmentsByStudent
  , listEnrollmentsBySection
  ) where

import           Data.Time (Day)
import           Database.MySQL.Simple

enrollStudent :: Connection -> Int -> Int -> Day -> IO ()
enrollStudent conn studentId sectionId dateD = do
  _ <- execute conn
    "INSERT INTO enrollments(student_id, section_id, enrolled_at) VALUES (?,?,?)"
    (studentId, sectionId, dateD)
  pure ()

unenrollStudent :: Connection -> Int -> Int -> IO ()
unenrollStudent conn studentId sectionId = do
  _ <- execute conn
    "DELETE FROM enrollments WHERE student_id=? AND section_id=?"
    (studentId, sectionId)
  pure ()

listEnrollmentsByStudent :: Connection -> Int -> IO [(Int, Int, Day)]
listEnrollmentsByStudent conn sid = query conn
  "SELECT student_id, section_id, enrolled_at FROM enrollments WHERE student_id=?"
  (Only sid)

listEnrollmentsBySection :: Connection -> Int -> IO [(Int, Int, Day)]
listEnrollmentsBySection conn secId = query conn
  "SELECT student_id, section_id, enrolled_at FROM enrollments WHERE section_id=?"
  (Only secId)
