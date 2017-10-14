module Utility where

import Atman.Prelude
import           Database.Persist.Postgresql
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import System.Environment (lookupEnv)


runAction :: ReaderT SqlBackend (ResourceT (LoggingT IO)) a -> IO a
runAction action = do
  connStr <- fromMaybe "host=localhost dbname=atman user=atman password=testing port=5432"
         <$> lookupEnv "DB_CONN"
  runStdoutLoggingT $ withPostgresqlPool (toS connStr) 10 $ \pool ->
    runResourceT $ runSqlPool action pool
