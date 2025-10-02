{-# LANGUAGE OverloadedStrings #-}
module UseCases.Sections
  ( listSections, addSection, deleteSection, editSection
  ) where

import           Data.Text (Text)
import           Database.MySQL.Simple
import           Domain.Section
import           Infra.DB.Sql (q, toSection)

listSections :: Connection -> IO [Section]
listSections conn = do
  rows <- q conn "SELECT id, name, description FROM sections ORDER BY name" ()
  pure (fmap toSection rows)

addSection :: Connection -> Text -> Text -> IO ()
addSection conn nm descr = do
  _ <- execute conn "INSERT INTO sections(name, description) VALUES (?,?)" (nm, descr)
  pure ()

deleteSection :: Connection -> Int -> IO ()
deleteSection conn sid = do
  _ <- execute conn "DELETE FROM sections WHERE id=?" (Only sid)
  pure ()

-- EDIT
editSection :: Connection -> Int -> Maybe Text -> Maybe Text -> IO ()
editSection conn sid mname mdescr = do
  let sql = "UPDATE sections \
            \SET name = COALESCE(?, name), \
            \    description = COALESCE(?, description) \
            \WHERE id=?"
  _ <- execute conn sql (mname, mdescr, sid)
  pure ()
