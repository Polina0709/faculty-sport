{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Infra.DB.Sql
  ( q, qe
  , toStudent, toTeacher, toSection, toSchedule, toCompetition
  ) where

import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Data.Time                                   (Day)
import           Database.MySQL.Simple                       (Connection, Query, query, execute)
import           Database.MySQL.Simple.QueryParams           (QueryParams)
import           Database.MySQL.Simple.QueryResults          (QueryResults)

import           Domain.Student                              (Student(..))
import           Domain.Teacher                              (Teacher(..))
import           Domain.Section                              (Section(..))
import           Domain.Schedule                             (Schedule(..))
import           Domain.Competition                          (Competition(..))

-- Узагальнені хелпери поверх mysql-simple
q :: (QueryParams p, QueryResults r) => Connection -> Query -> p -> IO [r]
q = query

qe :: (QueryParams p) => Connection -> Query -> p -> IO ()
qe conn sql params = do
  _ <- execute conn sql params
  pure ()

-- Мапери з рядків БД у доменні типи

toStudent :: (Int, Text, Text, Text, Day, Text) -> Student
toStudent (sid, f, l, grp, bday, em) =
  Student
    { studentId = sid
    , firstName = f
    , lastName  = l
    , groupCode = grp
    , birthDate = bday
    , email     = em
    }

toTeacher :: (Int, Text, Text, Text, Text) -> Teacher
toTeacher (tid, f, l, pos, em) =
  Teacher
    { teacherId = tid
    , tFirst    = f
    , tLast     = l
    , position  = pos
    , tEmail    = em
    }

toSection :: (Int, Text, Text) -> Section
toSection (sid, nm, descr) =
  Section
    { sectionId   = sid
    , sectionName = nm
    , description = descr
    }

-- ВАЖЛИВО: start_time/end_time форматуємо у SQL як текст (DATE_FORMAT('%H:%i:%s')),
-- тому тут приймаємо Text і конвертуємо у String для доменного типу.
toSchedule :: (Int, Int, Int, Text, Text, Int) -> Schedule
toSchedule (sid, secId, wd, st, et, locId) =
  Schedule
    { scheduleId = sid
    , sectionId  = secId
    , weekday    = wd
    , startTime  = T.unpack st
    , endTime    = T.unpack et
    , locationId = locId
    }

toCompetition :: (Int, Text, Int, Day, Int) -> Competition
toCompetition (cid, ttl, secId, d, locId) =
  Competition
    { competitionId = cid
    , title         = ttl
    , sectionId     = secId
    , dateDay       = d
    , locationId    = locId
    }
