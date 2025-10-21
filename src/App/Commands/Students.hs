{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Students (studentsP, StudentsCmd(..), handleStudents) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import qualified Domain.Student as DS
import UseCases.Students

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

studentsP :: Parser StudentsCmd
studentsP = hsubparser $
  command "list" (info (pure StList) (progDesc "Список студентів")) <>
  command "add"  (info stAdd (progDesc "Додати студента")) <>
  command "del"  (info stDel (progDesc "Видалити студента")) <>
  command "edit" (info stEdit (progDesc "Редагувати студента"))
  where
    stAdd = StAdd
      <$> strOption (long "first" <> help "Ім'я")
      <*> strOption (long "last" <> help "Прізвище")
      <*> strOption (long "group" <> help "Група")
      <*> strOption (long "email" <> help "Email")
      <*> strOption (long "birth" <> help "Дата народження YYYY-MM-DD")
    stDel = StDel <$> option auto (long "id" <> help "ID студента")
    stEdit = StEdit
      <$> option auto (long "id" <> help "ID студента")
      <*> optional (strOption (long "first"))
      <*> optional (strOption (long "last"))
      <*> optional (strOption (long "group"))
      <*> optional (strOption (long "email"))
      <*> optional (strOption (long "birth"))

handleStudents :: DbPool -> StudentsCmd -> IO ()
handleStudents pool cmd =
  withConn pool $ \conn -> case cmd of
    StList -> do
      xs <- listStudents conn
      if null xs then putStrLn "Студентів немає."
      else mapM_ (\s -> putStrLn $
           show (DS.studentId s) <> " | "
        <> T.unpack (DS.firstName s <> " " <> DS.lastName s)
        <> " | " <> T.unpack (DS.groupCode s)) xs
    StAdd f l g e b -> addStudent conn (T.pack f) (T.pack l) (T.pack g) (T.pack e) (read b)
                    >> putStrLn "✅ Студента додано."
    StDel i -> deleteStudent conn i >> putStrLn "🗑️  Студента видалено."
    StEdit sid mf ml mg me mb -> editStudent conn sid
                                (fmap T.pack mf) (fmap T.pack ml)
                                (fmap T.pack mg) (fmap T.pack me)
                                (fmap read mb)
                             >> putStrLn "✏️  Студента оновлено."
