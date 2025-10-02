{-# LANGUAGE OverloadedStrings #-}
module UseCases.Competitions
  ( listCompetitions
  , listCompetitionsBySection
  , addCompetition
  , deleteCompetition
  , listCompetitionsPretty
  , editCompetition
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day)
import           Database.MySQL.Simple
import           Infra.DB.Sql (q, toCompetition)
import           Domain.Competition

listCompetitions :: Connection -> IO [Competition]
listCompetitions conn = do
  rows <- q conn
    "SELECT id, title, section_id, date_day, location_id \
    \FROM competitions ORDER BY date_day" ()
  pure (fmap toCompetition rows)

listCompetitionsBySection :: Connection -> Int -> IO [Competition]
listCompetitionsBySection conn secId = do
  rows <- q conn
    "SELECT id, title, section_id, date_day, location_id \
    \FROM competitions WHERE section_id=? ORDER BY date_day" (Only secId)
  pure (fmap toCompetition rows)

addCompetition :: Connection -> Text -> Int -> Day -> Int -> IO ()
addCompetition conn ttl secId d locId = do
  _ <- execute conn
    "INSERT INTO competitions(title, section_id, date_day, location_id) \
    \VALUES (?,?,?,?)" (ttl, secId, d, locId)
  pure ()

deleteCompetition :: Connection -> Int -> IO ()
deleteCompetition conn cid = do
  _ <- execute conn "DELETE FROM competitions WHERE id=?" (Only cid)
  pure ()

listCompetitionsPretty :: Connection -> IO [(Int, Text, Day, Text, Text)]
listCompetitionsPretty conn = do
  let sql = "SELECT c.id, c.title, c.date_day, s.name AS section_name, l.name AS location_name \
            \FROM competitions c \
            \JOIN sections s   ON s.id = c.section_id \
            \JOIN locations l  ON l.id = c.location_id \
            \ORDER BY c.date_day"
  q conn sql ()

-- EDIT
editCompetition :: Connection
                -> Int            -- ^ competition id
                -> Maybe Text     -- ^ title
                -> Maybe Int      -- ^ section_id
                -> Maybe Day      -- ^ date_day
                -> Maybe Int      -- ^ location_id
                -> IO ()
editCompetition conn cid mttl msec mday mloc = do
  let sql = "UPDATE competitions \
            \SET title = COALESCE(?, title), \
            \    section_id = COALESCE(?, section_id), \
            \    date_day = COALESCE(?, date_day), \
            \    location_id = COALESCE(?, location_id) \
            \WHERE id=?"
  _ <- execute conn sql (mttl, msec, mday, mloc, cid)
  pure ()
