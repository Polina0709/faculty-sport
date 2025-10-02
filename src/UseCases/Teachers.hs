{-# LANGUAGE OverloadedStrings #-}
module UseCases.Teachers
  ( listTeachers, addTeacher, deleteTeacher, editTeacher
  ) where

import           Data.Text (Text)
import           Database.MySQL.Simple
import           Domain.Teacher
import           Infra.DB.Sql (q, toTeacher)

listTeachers :: Connection -> IO [Teacher]
listTeachers conn = do
  rows <- q conn
    "SELECT id, first_name, last_name, position, email \
    \FROM teachers ORDER BY last_name, first_name" ()
  pure (fmap toTeacher rows)

addTeacher :: Connection -> Text -> Text -> Text -> Text -> IO ()
addTeacher conn f l pos em = do
  _ <- execute conn
    "INSERT INTO teachers(first_name, last_name, position, email) \
    \VALUES (?,?,?,?)" (f,l,pos,em)
  pure ()

deleteTeacher :: Connection -> Int -> IO ()
deleteTeacher conn tid = do
  _ <- execute conn "DELETE FROM teachers WHERE id=?" (Only tid)
  pure ()

-- EDIT
editTeacher :: Connection -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
editTeacher conn tid mf ml mpos mem = do
  let sql = "UPDATE teachers \
            \SET first_name = COALESCE(?, first_name), \
            \    last_name  = COALESCE(?, last_name), \
            \    position   = COALESCE(?, position), \
            \    email      = COALESCE(?, email) \
            \WHERE id=?"
  _ <- execute conn sql (mf, ml, mpos, mem, tid)
  pure ()