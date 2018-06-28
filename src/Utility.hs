module Utility where

import           Application
import           Atman
import           Atman.Model
import           Atman.Prelude
import           Control.Monad.Except         (runExceptT)
import           Control.Monad.Logger         (LoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Database.Persist.Postgresql
import           Foundation
import           Prelude                      (userError)
import           System.Environment           (lookupEnv)


runAction :: ReaderT SqlBackend (ResourceT (LoggingT IO)) a -> IO a
runAction action = do
  connStr <- fromMaybe "host=localhost dbname=atman user=atman password=testing port=5432"
         <$> lookupEnv "DB_CONN"
  runStdoutLoggingT $ withPostgresqlPool (toS connStr) 10 $ \pool ->
    runResourceT $ runSqlPool action pool

runAtman :: Atman a -> IO (Either Text a)
runAtman action =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do
    app <- makeFoundation pool
    runExceptT . flip runReaderT app . runResourceT $ action
  where
    connStr = "host=localhost dbname=atman user=atman password=testing port=5432"

getAtmanUser = do
  muser <- runAction $ getBy (Username "atman_user")
  case muser of
    Nothing      -> throwIO $ userError "User not found"
    (Just euser) -> return . entityVal $ euser
