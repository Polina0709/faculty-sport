{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Coaches (coachesP, CoachesCmd(..), handleCoaches) where

import Options.Applicative
import qualified Data.Text as T
import Infra.DB.Pool (DbPool, withConn)
import UseCases.Coaches

data CoachesCmd
  = CAddCoach { cTeacher :: Int, cSection :: Int }
  | CDelCoach { cdTeacher :: Int, cdSection :: Int }
  | CListBySectionCmd { clsSection :: Int }
  | CListByTeacherCmd { cltTeacher :: Int }
  deriving (Show)

coachesP :: Parser CoachesCmd
coachesP = hsubparser $
  command "add" (info cAdd (progDesc "Закріпити викладача")) <>
  command "del" (info cDel (progDesc "Відкріпити викладача")) <>
  command "list-by-section" (info cListS (progDesc "Список викладачів секції")) <>
  command "list-by-teacher" (info cListT (progDesc "Список секцій викладача"))
  where
    cAdd = CAddCoach <$> option auto (long "teacher-id") <*> option auto (long "section-id")
    cDel = CDelCoach <$> option auto (long "teacher-id") <*> option auto (long "section-id")
    cListS = CListBySectionCmd <$> option auto (long "section-id")
    cListT = CListByTeacherCmd <$> option auto (long "teacher-id")

handleCoaches :: DbPool -> CoachesCmd -> IO ()
handleCoaches pool cmd =
  withConn pool $ \conn -> case cmd of
    CAddCoach t s -> addCoach conn t s >> putStrLn "✅ Викладача закріплено."
    CDelCoach t s -> deleteCoach conn t s >> putStrLn "🗑️  Відкріплено."
    CListBySectionCmd s -> listCoachesBySectionPretty conn s >>= mapM_ (\(tid, tname) ->
      putStrLn $ show tid <> " | " <> T.unpack tname)
    CListByTeacherCmd t -> listCoachesByTeacherPretty conn t >>= mapM_ (\(sid, sname) ->
      putStrLn $ show sid <> " | " <> T.unpack sname)
