module Infra.DB.Pool
  ( DbPool
  , withPool
  , withConn
  ) where

import           Control.Monad          (void)
import           Data.Pool              (Pool, createPool, withResource, destroyAllResources)
import           Database.MySQL.Simple  (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import           System.Environment     (lookupEnv)
import           Data.Maybe             (fromMaybe)
import           Text.Read              (readMaybe)
import           Data.Time              (NominalDiffTime)

type DbPool = Pool Connection

readEnv :: Read a => String -> a -> IO a
readEnv key def = do
  mv <- lookupEnv key
  pure $ maybe def id (mv >>= readMaybe)

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "DB_HOST"
  user <- fromMaybe "fs_user"   <$> lookupEnv "DB_USER"
  pass <- fromMaybe "fs_pass" <$> lookupEnv "DB_PASS"
  db   <- fromMaybe "faculty_sport"   <$> lookupEnv "DB_NAME"
  pure defaultConnectInfo
      { connectHost     = host
      , connectUser     = user
      , connectPassword = pass
      , connectDatabase = db
      }

withPool :: (DbPool -> IO a) -> IO a
withPool action = do
  ci <- getConnectInfo
  stripes  <- readEnv "DB_POOL_STRIPES" 1
  perStripe<- readEnv "DB_POOL_RES"     10
  idleSec  <- readEnv "DB_POOL_IDLE"    60
  pool <- createPool (connect ci) close stripes (fromIntegral (idleSec :: Int) :: NominalDiffTime) perStripe
  r <- action pool
  destroyAllResources pool
  pure r

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn = withResource
