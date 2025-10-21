{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Teachers (teachersP, TeachersCmd(..), handleTeachers) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import qualified Domain.Teacher as DT
import UseCases.Teachers

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

teachersP :: Parser TeachersCmd
teachersP = hsubparser $
  command "list" (info (pure TList) (progDesc "Список викладачів")) <>
  command "add"  (info tAdd (progDesc "Додати викладача")) <>
  command "del"  (info tDel (progDesc "Видалити викладача")) <>
  command "edit" (info tEdit (progDesc "Редагувати викладача"))
  where
    tAdd = TAdd
      <$> strOption (long "first" <> help "Ім'я")
      <*> strOption (long "last" <> help "Прізвище")
      <*> strOption (long "pos"  <> help "Посада")
      <*> strOption (long "email"<> help "Email")
    tDel = TDel <$> option auto (long "id" <> help "ID викладача")
    tEdit = TEdit
      <$> option auto (long "id" <> help "ID викладача")
      <*> optional (strOption (long "first"))
      <*> optional (strOption (long "last"))
      <*> optional (strOption (long "pos"))
      <*> optional (strOption (long "email"))

handleTeachers :: DbPool -> TeachersCmd -> IO ()
handleTeachers pool cmd =
  withConn pool $ \conn -> case cmd of
    TList -> do
      xs <- listTeachers conn
      if null xs then putStrLn "Викладачів немає."
      else mapM_ (\t -> putStrLn $
           show (DT.teacherId t) <> " | "
        <> T.unpack (DT.tFirst t <> " " <> DT.tLast t)
        <> " | " <> T.unpack (DT.position t)) xs
    TAdd f l p e -> addTeacher conn (T.pack f) (T.pack l) (T.pack p) (T.pack e)
                  >> putStrLn "✅ Викладача додано."
    TDel i -> deleteTeacher conn i >> putStrLn "🗑️  Викладача видалено."
    TEdit tid mf ml mpos mem ->
      editTeacher conn tid (fmap T.pack mf) (fmap T.pack ml)
                  (fmap T.pack mpos) (fmap T.pack mem)
               >> putStrLn "✏️  Викладача оновлено."
