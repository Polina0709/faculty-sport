{-# LANGUAGE OverloadedStrings #-}

module App.CLI (runCLI) where

import Options.Applicative
import Infra.DB.Pool (withPool)

-- Імпорт усіх груп команд разом із типами
import App.Commands.Students     (StudentsCmd(..), studentsP, handleStudents)
import App.Commands.Teachers     (TeachersCmd(..), teachersP, handleTeachers)
import App.Commands.Sections     (SectionsCmd(..), sectionsP, handleSections)
import App.Commands.Schedule     (ScheduleCmd(..), scheduleP, handleSchedule)
import App.Commands.Competitions (CompetitionsCmd(..), competitionsP, handleCompetitions)
import App.Commands.Enrollments  (EnrollCmd(..), enrollP, handleEnroll)
import App.Commands.Locations    (LocationsCmd(..), locationsP, handleLocations)
import App.Commands.Coaches      (CoachesCmd(..), coachesP, handleCoaches)

--------------------------------------------------------------------------------

data Command
  = CmdPingDB
  | CmdStudents StudentsCmd
  | CmdTeachers TeachersCmd
  | CmdSections SectionsCmd
  | CmdSchedule ScheduleCmd
  | CmdCompetitions CompetitionsCmd
  | CmdEnroll EnrollCmd
  | CmdLocations LocationsCmd
  | CmdCoaches CoachesCmd
  deriving (Show)

--------------------------------------------------------------------------------
-- CLI parser

parseCmd :: Parser Command
parseCmd = hsubparser $
     command "ping-db" (info (pure CmdPingDB) (progDesc "Перевірити підключення до БД"))
  <> command "students" (info (CmdStudents <$> studentsP) (progDesc "Операції зі студентами"))
  <> command "teachers" (info (CmdTeachers <$> teachersP) (progDesc "Операції з викладачами"))
  <> command "sections" (info (CmdSections <$> sectionsP) (progDesc "Операції з секціями"))
  <> command "schedule" (info (CmdSchedule <$> scheduleP) (progDesc "Розклад занять"))
  <> command "competitions" (info (CmdCompetitions <$> competitionsP) (progDesc "Змагання"))
  <> command "enroll" (info (CmdEnroll <$> enrollP) (progDesc "Записи студентів у секції"))
  <> command "locations" (info (CmdLocations <$> locationsP) (progDesc "Локації"))
  <> command "coaches" (info (CmdCoaches <$> coachesP) (progDesc "Викладачі/тренери у секціях"))

opts :: ParserInfo Command
opts = info (parseCmd <**> helper)
  (fullDesc <> progDesc "Faculty Sport CLI" <> header "faculty-sport CLI")

--------------------------------------------------------------------------------
-- Runner

runCLI :: IO ()
runCLI = do
  cmd <- execParser opts
  withPool $ \pool -> case cmd of
    CmdPingDB         -> putStrLn "✅ Підключення до БД успішне."
    CmdStudents sc     -> handleStudents pool sc
    CmdTeachers tc     -> handleTeachers pool tc
    CmdSections sc     -> handleSections pool sc
    CmdSchedule sch    -> handleSchedule pool sch
    CmdCompetitions cc -> handleCompetitions pool cc
    CmdEnroll ec       -> handleEnroll pool ec
    CmdLocations lc    -> handleLocations pool lc
    CmdCoaches cc      -> handleCoaches pool cc
