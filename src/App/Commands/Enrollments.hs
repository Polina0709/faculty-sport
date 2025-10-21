{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Enrollments (enrollP, EnrollCmd(..), handleEnroll) where

import Options.Applicative
import Infra.DB.Pool (DbPool, withConn)
import UseCases.Enrollments

data EnrollCmd
  = Enroll { estudent :: Int, esection :: Int, edate :: String }
  | Unenroll { ustudent :: Int, usection :: Int }
  | EnrollListByStudent { lstudent :: Int }
  | EnrollListBySection { lsection :: Int }
  deriving (Show)

enrollP :: Parser EnrollCmd
enrollP = hsubparser $
  command "add" (info enAdd (progDesc "Записати студента")) <>
  command "del" (info enDel (progDesc "Відписати студента")) <>
  command "list-by-student" (info enListS (progDesc "Записи студента")) <>
  command "list-by-section" (info enListSec (progDesc "Записи секції"))
  where
    enAdd = Enroll <$> option auto (long "student-id") <*> option auto (long "section-id") <*> strOption (long "date")
    enDel = Unenroll <$> option auto (long "student-id") <*> option auto (long "section-id")
    enListS = EnrollListByStudent <$> option auto (long "student-id")
    enListSec = EnrollListBySection <$> option auto (long "section-id")

handleEnroll :: DbPool -> EnrollCmd -> IO ()
handleEnroll pool cmd =
  withConn pool $ \conn -> case cmd of
    Enroll sid sec d -> enrollStudent conn sid sec (read d) >> putStrLn "✅ Запис створено."
    Unenroll sid sec -> unenrollStudent conn sid sec >> putStrLn "🗑️  Видалено."
    EnrollListByStudent sid -> listEnrollmentsByStudent conn sid >>= mapM_ print
    EnrollListBySection sec -> listEnrollmentsBySection conn sec >>= mapM_ print
