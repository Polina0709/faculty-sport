{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Options.Applicative (optional)
import           Data.Semigroup                  ((<>))
import qualified Data.Text                       as T
import           Data.Text                       (Text)
import           Data.Time                       (Day)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)

import           Infra.DB.Pool                   (withPool, withConn)

-- UseCases
import           UseCases.Students
import           UseCases.Teachers
import           UseCases.Sections
import           UseCases.Schedule
import           UseCases.Competitions
import           UseCases.Enrollments
import           UseCases.Locations
import           UseCases.Coaches

-- Domain (кваліфіковані імпорти, щоб уникнути колізій полів)
import qualified Domain.Student                  as DS
import qualified Domain.Teacher                  as DT
import qualified Domain.Section                  as DSec
import qualified Domain.Schedule                 as DSch
import qualified Domain.Competition              as DC
import qualified Domain.Location                 as DL

--------------------------------------------------------------------------------
-- Команди

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

data StudentsCmd
  = StList
  | StAdd { sf :: String, sl :: String, sgroup :: String, semail :: String, sbirth :: String }
  | StDel { sid :: Int }
  | StEdit { seid :: Int
           , sef  :: Maybe String
           , sel  :: Maybe String
           , segr :: Maybe String
           , seem :: Maybe String
           , seb  :: Maybe String
           }
  deriving (Show)

data TeachersCmd
  = TList
  | TAdd { tf :: String, tl :: String, tpos :: String, temail :: String }
  | TDel { tid :: Int }
  | TEdit { teid :: Int
          , tef  :: Maybe String
          , tel  :: Maybe String
          , tepos:: Maybe String
          , teem :: Maybe String
          }
  deriving (Show)

data SectionsCmd
  = SList
  | SAdd { sname :: String, sdescr :: String }
  | SDel { secid :: Int }
  | SEdit { seid2 :: Int, snameE :: Maybe String, sdescrE :: Maybe String }
  deriving (Show)

data ScheduleCmd
  = SchList { secIdForList :: Int }
  | SchAdd  { secIdA :: Int, wdA :: Int, stA :: String, etA :: String, locA :: Int }
  | SchEdit { schIdE :: Int, stE :: String, etE :: String }
  | SchDel  { schIdD :: Int }
  deriving (Show)

data CompetitionsCmd
  = CList
  | CListPretty
  | CListBySec { csec :: Int }
  | CAdd { ctitle :: String, csecid :: Int, cdate :: String, cloc :: Int }
  | CDel { cidDel :: Int }
  | CEdit { ceid :: Int
          , cetitle :: Maybe String
          , cesecid :: Maybe Int
          , cedate  :: Maybe String
          , celoc   :: Maybe Int
          }
  deriving (Show)

data EnrollCmd
  = Enroll { estudent :: Int, esection :: Int, edate :: String }
  | Unenroll { ustudent :: Int, usection :: Int }
  | EnrollListByStudent { lstudent :: Int }
  | EnrollListBySection { lsection :: Int }
  deriving (Show)

data LocationsCmd
  = LList
  | LAdd { lname :: String, laddr :: String }
  | LDel { lidDel :: Int }
  | LEdit { leid :: Int, lnameE :: Maybe String, laddrE :: Maybe String }
  deriving (Show)

data CoachesCmd
  = CAddCoach { cTeacher :: Int, cSection :: Int }
  | CDelCoach { cdTeacher :: Int, cdSection :: Int }
  | CListBySectionCmd { clsSection :: Int }
  | CListByTeacherCmd { cltTeacher :: Int }
  deriving (Show)

--------------------------------------------------------------------------------
-- Парсери CLI

parseCmd :: Parser Command
parseCmd = hsubparser $
  command "ping-db"      (info (pure CmdPingDB)                    (progDesc "Перевірити підключення до БД")) <>
  command "students"     (info (CmdStudents    <$> studentsP)      (progDesc "Операції зі студентами"))       <>
  command "teachers"     (info (CmdTeachers    <$> teachersP)      (progDesc "Операції з викладачами"))      <>
  command "sections"     (info (CmdSections    <$> sectionsP)      (progDesc "Операції з секціями"))         <>
  command "schedule"     (info (CmdSchedule    <$> scheduleP)      (progDesc "Розклад секцій"))              <>
  command "competitions" (info (CmdCompetitions <$> competitionsP) (progDesc "Змагання"))                    <>
  command "enroll"       (info (CmdEnroll      <$> enrollP)        (progDesc "Записи студентів у секції"))   <>
  command "locations"    (info (CmdLocations   <$> locationsP)     (progDesc "Локації для розкладу/змагань"))<>
  command "coaches"      (info (CmdCoaches     <$> coachesP)       (progDesc "Викладачі, закріплені за секціями"))

studentsP :: Parser StudentsCmd
studentsP = hsubparser $
  command "list" (info (pure StList) (progDesc "Список студентів")) <>
  command "add"  (info stAdd         (progDesc "Додати студента"))  <>
  command "del"  (info stDel         (progDesc "Видалити студента"))<>
  command "edit" (info stEdit        (progDesc "Редагувати студента"))
  where
    stAdd = StAdd
      <$> strOption (long "first" <> metavar "FIRST" <> help "Ім'я")
      <*> strOption (long "last"  <> metavar "LAST"  <> help "Прізвище")
      <*> strOption (long "group" <> metavar "GROUP" <> help "Група")
      <*> strOption (long "email" <> metavar "EMAIL" <> help "Email")
      <*> strOption (long "birth" <> metavar "YYYY-MM-DD" <> help "Дата народження")
    stDel = StDel <$> option auto (long "id" <> metavar "ID" <> help "ID студента")
    stEdit = StEdit
      <$> option auto (long "id" <> metavar "ID" <> help "ID студента")
      <*> optional (strOption (long "first" <> metavar "FIRST"))
      <*> optional (strOption (long "last"  <> metavar "LAST"))
      <*> optional (strOption (long "group" <> metavar "GROUP"))
      <*> optional (strOption (long "email" <> metavar "EMAIL"))
      <*> optional (strOption (long "birth" <> metavar "YYYY-MM-DD"))

teachersP :: Parser TeachersCmd
teachersP = hsubparser $
  command "list" (info (pure TList) (progDesc "Список викладачів")) <>
  command "add"  (info tAdd         (progDesc "Додати викладача"))  <>
  command "del"  (info tDel         (progDesc "Видалити викладача"))<>
  command "edit" (info tEdit        (progDesc "Редагувати викладача"))
  where
    tAdd = TAdd
      <$> strOption (long "first" <> metavar "FIRST" <> help "Ім'я")
      <*> strOption (long "last"  <> metavar "LAST"  <> help "Прізвище")
      <*> strOption (long "pos"   <> metavar "POSITION" <> help "Посада")
      <*> strOption (long "email" <> metavar "EMAIL" <> help "Email")
    tDel = TDel <$> option auto (long "id" <> metavar "ID" <> help "ID викладача")
    tEdit = TEdit
      <$> option auto (long "id" <> metavar "ID")
      <*> optional (strOption (long "first" <> metavar "FIRST"))
      <*> optional (strOption (long "last"  <> metavar "LAST"))
      <*> optional (strOption (long "pos"   <> metavar "POSITION"))
      <*> optional (strOption (long "email" <> metavar "EMAIL"))

sectionsP :: Parser SectionsCmd
sectionsP = hsubparser $
  command "list" (info (pure SList) (progDesc "Список секцій")) <>
  command "add"  (info sAdd         (progDesc "Додати секцію")) <>
  command "del"  (info sDel         (progDesc "Видалити секцію"))<>
  command "edit" (info sEdit        (progDesc "Редагувати секцію"))
  where
    sAdd = SAdd
      <$> strOption (long "name"  <> metavar "NAME"  <> help "Назва секції")
      <*> strOption (long "descr" <> metavar "DESCR" <> help "Опис")
    sDel = SDel <$> option auto (long "id" <> metavar "ID" <> help "ID секції")
    sEdit = SEdit
      <$> option auto (long "id" <> metavar "ID")
      <*> optional (strOption (long "name"  <> metavar "NAME"))
      <*> optional (strOption (long "descr" <> metavar "DESCR"))

scheduleP :: Parser ScheduleCmd
scheduleP = hsubparser $
  command "list" (info schList (progDesc "Список розкладу секції (з назвами локацій)")) <>
  command "add"  (info schAdd  (progDesc "Додати слот розкладу"))  <>
  command "edit" (info schEdit (progDesc "Змінити час слоту"))     <>
  command "del"  (info schDel  (progDesc "Видалити слот"))
  where
    schList = SchList <$> option auto (long "section-id" <> metavar "SECTION_ID" <> help "ID секції")
    schAdd  = SchAdd
      <$> option auto (long "section-id" <> metavar "SECTION_ID" <> help "ID секції")
      <*> option auto (long "weekday"    <> metavar "1..7"       <> help "День тижня (1=Mon)")
      <*> strOption   (long "start"      <> metavar "HH:MM:SS"   <> help "Початок")
      <*> strOption   (long "end"        <> metavar "HH:MM:SS"   <> help "Кінець")
      <*> option auto (long "location-id"<> metavar "LOCATION_ID"<> help "Локація")
    schEdit = SchEdit
      <$> option auto (long "id"    <> metavar "SCHEDULE_ID" <> help "ID слоту")
      <*> strOption   (long "start" <> metavar "HH:MM:SS"     <> help "Новий початок")
      <*> strOption   (long "end"   <> metavar "HH:MM:SS"     <> help "Новий кінець")
    schDel  = SchDel <$> option auto (long "id" <> metavar "SCHEDULE_ID" <> help "ID слоту")

competitionsP :: Parser CompetitionsCmd
competitionsP = hsubparser $
  command "list"            (info (pure CList)            (progDesc "Список змагань")) <>
  command "list-pretty"     (info (pure CListPretty)      (progDesc "Список змагань (із секцією та локацією)")) <>
  command "list-by-section" (info cListBySec              (progDesc "Список змагань секції")) <>
  command "add"             (info cAdd                    (progDesc "Додати змагання")) <>
  command "del"             (info cDel                    (progDesc "Видалити змагання")) <>
  command "edit"            (info cEdit                   (progDesc "Редагувати змагання"))
  where
    cListBySec = CListBySec <$> option auto (long "section-id" <> metavar "SECTION_ID" <> help "ID секції")
    cAdd = CAdd
      <$> strOption   (long "title"       <> metavar "TITLE"       <> help "Назва")
      <*> option auto (long "section-id"  <> metavar "SECTION_ID"  <> help "ID секції")
      <*> strOption   (long "date"        <> metavar "YYYY-MM-DD"  <> help "Дата")
      <*> option auto (long "location-id" <> metavar "LOCATION_ID" <> help "Локація")
    cDel = CDel <$> option auto (long "id" <> metavar "ID" <> help "ID змагання")
    cEdit = CEdit
      <$> option auto (long "id" <> metavar "ID")
      <*> optional (strOption   (long "title"       <> metavar "TITLE"))
      <*> optional (option auto (long "section-id"  <> metavar "SECTION_ID"))
      <*> optional (strOption   (long "date"        <> metavar "YYYY-MM-DD"))
      <*> optional (option auto (long "location-id" <> metavar "LOCATION_ID"))

enrollP :: Parser EnrollCmd
enrollP = hsubparser $
  command "add"            (info enAdd     (progDesc "Записати студента у секцію"))      <>
  command "del"            (info enDel     (progDesc "Відписати студента від секції"))   <>
  command "list-by-student"(info enListS   (progDesc "Показати записи студента"))        <>
  command "list-by-section"(info enListSec (progDesc "Показати записи секції"))
  where
    enAdd = Enroll
      <$> option auto (long "student-id" <> metavar "STUDENT_ID" <> help "ID студента")
      <*> option auto (long "section-id" <> metavar "SECTION_ID" <> help "ID секції")
      <*> strOption   (long "date"       <> metavar "YYYY-MM-DD" <> help "Дата запису")
    enDel = Unenroll
      <$> option auto (long "student-id" <> metavar "STUDENT_ID" <> help "ID студента")
      <*> option auto (long "section-id" <> metavar "SECTION_ID" <> help "ID секції")
    enListS   = EnrollListByStudent <$> option auto (long "student-id" <> metavar "STUDENT_ID" <> help "ID студента")
    enListSec = EnrollListBySection <$> option auto (long "section-id"  <> metavar "SECTION_ID"  <> help "ID секції")

locationsP :: Parser LocationsCmd
locationsP = hsubparser $
  command "list" (info (pure LList) (progDesc "Список локацій")) <>
  command "add"  (info lAdd         (progDesc "Додати локацію")) <>
  command "del"  (info lDel         (progDesc "Видалити локацію")) <>
  command "edit" (info lEdit        (progDesc "Редагувати локацію"))
  where
    lAdd = LAdd
      <$> strOption (long "name"  <> metavar "NAME"  <> help "Назва локації")
      <*> strOption (long "addr"  <> metavar "ADDR"  <> help "Адреса")
    lDel = LDel <$> option auto (long "id" <> metavar "ID" <> help "ID локації")
    lEdit = LEdit
      <$> option auto (long "id" <> metavar "ID")
      <*> optional (strOption (long "name" <> metavar "NAME"))
      <*> optional (strOption (long "addr" <> metavar "ADDR"))

coachesP :: Parser CoachesCmd
coachesP = hsubparser $
  command "add"  (info cAdd (progDesc "Закріпити викладача за секцією")) <>
  command "del"  (info cDel (progDesc "Відкріпити викладача від секції")) <>
  command "list-by-section" (info cListS (progDesc "Список викладачів секції")) <>
  command "list-by-teacher" (info cListT (progDesc "Список секцій викладача"))
  where
    cAdd  = CAddCoach <$> option auto (long "teacher-id" <> metavar "TID")
                      <*> option auto (long "section-id" <> metavar "SID")
    cDel  = CDelCoach <$> option auto (long "teacher-id" <> metavar "TID")
                      <*> option auto (long "section-id" <> metavar "SID")
    cListS = CListBySectionCmd <$> option auto (long "section-id" <> metavar "SID")
    cListT = CListByTeacherCmd <$> option auto (long "teacher-id" <> metavar "TID")

opts :: ParserInfo Command
opts = info (parseCmd <**> helper)
  ( fullDesc
 <> progDesc "Faculty Sport CLI"
 <> header   "faculty-sport — інфосистема Спорт на факультеті (CLI)"
  )

--------------------------------------------------------------------------------
-- Утиліти

parseDay :: String -> Either String Day
parseDay s =
  case parseTimeM True defaultTimeLocale "%F" s of
    Just d  -> Right d
    Nothing -> Left "Невірний формат дати, потрібно YYYY-MM-DD."

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  cmd <- execParser opts
  withPool $ \pool -> case cmd of
    CmdPingDB ->
      withConn pool $ \_ -> putStrLn "Підключення до БД успішне."

    CmdStudents sc ->
      withConn pool $ \conn -> case sc of
        StList -> do
          xs <- listStudents conn
          if null xs
            then putStrLn "Студентів немає."
            else mapM_ (\s -> putStrLn $
                   show (DS.studentId s) <> " | "
                <> T.unpack (DS.firstName s <> " " <> DS.lastName s) <> " | "
                <> T.unpack (DS.groupCode s)
                 ) xs
        StAdd f l g e b ->
          case parseDay b of
            Left err -> putStrLn err
            Right d  -> addStudent conn (T.pack f) (T.pack l) (T.pack g) (T.pack e) d
                      >> putStrLn "Студента додано."
        StDel i -> deleteStudent conn i >> putStrLn "Студента видалено."
        StEdit sid mf ml mg me mbS -> do
          mday <- case traverse parseDay mbS of
                    Left err     -> putStrLn err >> pure Nothing
                    Right mDayOk -> pure mDayOk
          editStudent conn sid (fmap T.pack mf) (fmap T.pack ml) (fmap T.pack mg) (fmap T.pack me) mday
          putStrLn "Студента оновлено."

    CmdTeachers tc ->
      withConn pool $ \conn -> case tc of
        TList -> do
          xs <- listTeachers conn
          if null xs
            then putStrLn "Викладачів немає."
            else mapM_ (\t -> putStrLn $
                   show (DT.teacherId t) <> " | "
                <> T.unpack (DT.tFirst t <> " " <> DT.tLast t) <> " | "
                <> T.unpack (DT.position t)
                 ) xs
        TAdd f l p e -> addTeacher conn (T.pack f) (T.pack l) (T.pack p) (T.pack e)
                      >> putStrLn "Викладача додано."
        TDel i       -> deleteTeacher conn i >> putStrLn "Викладача видалено."
        TEdit tid mf ml mpos mem -> do
          editTeacher conn tid (fmap T.pack mf) (fmap T.pack ml) (fmap T.pack mpos) (fmap T.pack mem)
          putStrLn "Викладача оновлено."

    CmdSections scmd ->
      withConn pool $ \conn -> case scmd of
        SList -> do
          xs <- listSections conn
          if null xs
            then putStrLn "Секцій немає."
            else mapM_ (\s -> putStrLn $
                   show (DSec.sectionId s) <> " | "
                <> T.unpack (DSec.sectionName s)
                 ) xs
        SAdd nm ds -> addSection conn (T.pack nm) (T.pack ds) >> putStrLn "Секцію додано."
        SDel i     -> deleteSection conn i >> putStrLn "Секцію видалено."
        SEdit sid mname mdescr -> do
          editSection conn sid (fmap T.pack mname) (fmap T.pack mdescr)
          putStrLn "Секцію оновлено."

    CmdSchedule scmd ->
      withConn pool $ \conn -> case scmd of
        SchList sid -> do
          xs <- listSchedulePretty conn sid
          if null xs
            then putStrLn "Розкладу немає."
            else mapM_ (\(slotId, wd, st, et, locName) -> putStrLn $
                   show slotId <> " | day " <> show wd
                <> " | " <> T.unpack st <> "-" <> T.unpack et
                <> " | " <> T.unpack locName
                 ) xs
        SchAdd sid wd st et loc -> addSchedule conn sid wd st et loc >> putStrLn "Слот додано."
        SchEdit sid st et       -> editScheduleTime conn sid st et   >> putStrLn "Слот оновлено."
        SchDel sid              -> deleteSchedule conn sid           >> putStrLn "Слот видалено."

    CmdCompetitions cc ->
      withConn pool $ \conn -> case cc of
        CList -> do
          xs <- listCompetitions conn
          if null xs
            then putStrLn "Змагань немає."
            else mapM_ (\c -> putStrLn $
                   show (DC.competitionId c) <> " | "
                <> T.unpack (DC.title c) <> " | "
                <> show (DC.dateDay c) <> " | sec " <> show (DC.sectionId c)
                 ) xs
        CListPretty -> do
          xs <- listCompetitionsPretty conn
          if null xs
            then putStrLn "Змагань немає."
            else mapM_ (\(cid, ttl, d, secName, locName) -> putStrLn $
                   show cid <> " | " <> T.unpack ttl <> " | " <> show d
                   <> " | секція: " <> T.unpack secName
                   <> " | локація: " <> T.unpack locName
                 ) xs
        CListBySec sid -> do
          xs <- listCompetitionsBySection conn sid
          if null xs
            then putStrLn "Змагань немає."
            else mapM_ (\c -> putStrLn $
                   show (DC.competitionId c) <> " | "
                <> T.unpack (DC.title c) <> " | "
                <> show (DC.dateDay c)
                 ) xs
        CAdd ttl sid d loc ->
          case parseDay d of
            Left err  -> putStrLn err
            Right dayV -> addCompetition conn (T.pack ttl) sid dayV loc
                       >> putStrLn "Змагання додано."
        CDel i -> deleteCompetition conn i >> putStrLn "Змагання видалено."
        CEdit cid mttl msec mdate mloc -> do
          mday <- case traverse parseDay mdate of
                    Left err     -> putStrLn err >> pure Nothing
                    Right mDayOk -> pure mDayOk
          editCompetition conn cid (fmap T.pack mttl) msec mday mloc
          putStrLn "Змагання оновлено."

    CmdEnroll ec ->
      withConn pool $ \conn -> case ec of
        Enroll sid sec d ->
          case parseDay d of
            Left err   -> putStrLn err
            Right dayV -> enrollStudent conn sid sec dayV >> putStrLn "Запис створено."
        Unenroll sid sec -> unenrollStudent conn sid sec >> putStrLn "Запис видалено."
        EnrollListByStudent sid -> do
          xs <- listEnrollmentsByStudent conn sid
          if null xs
            then putStrLn "Записів немає."
            else mapM_ (\(s,sec,dt) -> putStrLn $
                   "student " <> show s <> " -> section " <> show sec <> " (" <> show dt <> ")"
                 ) xs
        EnrollListBySection sec -> do
          xs <- listEnrollmentsBySection conn sec
          if null xs
            then putStrLn "Записів немає."
            else mapM_ (\(s,sec',dt) -> putStrLn $
                   "section " <> show sec' <> " <- student " <> show s <> " (" <> show dt <> ")"
                 ) xs

    CmdLocations lcmd ->
      withConn pool $ \conn -> case lcmd of
        LList -> do
          xs <- listLocations conn
          if null xs
            then putStrLn "Локацій немає."
            else mapM_ (\l -> putStrLn $
                   show (DL.locationId l) <> " | "
                <> T.unpack (DL.locationName l) <> " | "
                <> T.unpack (DL.address l)
                 ) xs
        LAdd nm addr -> addLocation conn (T.pack nm) (T.pack addr) >> putStrLn "Локацію додано."
        LDel i       -> deleteLocation conn i >> putStrLn "Локацію видалено."
        LEdit lid mname maddr -> do
          editLocation conn lid (fmap T.pack mname) (fmap T.pack maddr)
          putStrLn "Локацію оновлено."

    CmdCoaches cc ->
      withConn pool $ \conn -> case cc of
        CAddCoach t s -> addCoach conn t s >> putStrLn "Викладача закріплено за секцією."
        CDelCoach t s -> deleteCoach conn t s >> putStrLn "Викладача відкріплено."
        CListBySectionCmd s -> do
          xs <- listCoachesBySectionPretty conn s
          if null xs then putStrLn "Для секції викладачів не знайдено."
                     else mapM_ (\(tid, tname) -> putStrLn $ show tid <> " | " <> T.unpack tname) xs
        CListByTeacherCmd t -> do
          xs <- listCoachesByTeacherPretty conn t
          if null xs then putStrLn "Секцій для викладача не знайдено."
                     else mapM_ (\(sid, sname) -> putStrLn $ show sid <> " | " <> T.unpack sname) xs

