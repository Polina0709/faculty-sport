{-# LANGUAGE OverloadedStrings #-}

module UseCases.Students
  ( listStudents, addStudent, deleteStudent, editStudent )
where


import           Data.Text                  (Text)
import           Data.Time                  (Day)
import           Database.MySQL.Simple
import           Domain.Student
import           Infra.DB.Sql               (q, qe, toStudent)

-- SELECT id, first_name, last_name, group_code, birthdate, email
listStudents :: Connection -> IO [Student]
listStudents conn = do
  rows <- q conn
            "SELECT id, first_name, last_name, group_code, birthdate, email \
            \FROM students ORDER BY last_name, first_name"
            ()         -- <— важливо: порожні параметри
  pure (fmap toStudent rows)

-- INSERT
addStudent :: Connection
           -> Text   -- first
           -> Text   -- last
           -> Text   -- group
           -> Text   -- email
           -> Day    -- birthdate
           -> IO ()
addStudent conn f l grp em bday = do
  _ <- execute conn
        "INSERT INTO students(first_name, last_name, group_code, birthdate, email) \
        \VALUES (?,?,?,?,?)"
        (f, l, grp, bday, em)
  pure ()

-- DELETE
deleteStudent :: Connection -> Int -> IO ()
deleteStudent conn sid = do
  _ <- execute conn "DELETE FROM students WHERE id=?" (Only sid)
  pure ()

-- EDIT
editStudent :: Connection
            -> Int                 -- ^ student id
            -> Maybe Text          -- ^ first_name
            -> Maybe Text          -- ^ last_name
            -> Maybe Text          -- ^ group_code
            -> Maybe Text          -- ^ email
            -> Maybe Day           -- ^ birthdate
            -> IO ()
editStudent conn sid mf ml mg me mb = do
  let sql = "UPDATE students \
            \SET first_name = COALESCE(?, first_name), \
            \    last_name  = COALESCE(?, last_name), \
            \    group_code = COALESCE(?, group_code), \
            \    email      = COALESCE(?, email), \
            \    birthdate  = COALESCE(?, birthdate) \
            \WHERE id=?"
  _ <- execute conn sql (mf, ml, mg, me, mb, sid)
  pure ()
