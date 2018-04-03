import           Application                  (makeFoundation)
import qualified Atman                     as Atman
import           Atman.Model
import           Atman.Prelude
import           Control.Monad.Logger         (runStdoutLoggingT)
import           Control.Monad.Trans.Control  (control)
import           Control.Monad.Trans.Resource (runResourceT)
import           Database.Persist.Postgresql
import           Foundation
import           System.Environment           (lookupEnv)
import           System.Posix.Signals         (Handler (Catch), installHandler,
                                               keyboardSignal)
import           Yesod.Core

main ∷ IO ()
main = do
  connStr <- maybe "host=localhost dbname=atman user=atman password=testing port=5432" identity <$> lookupEnv "DB_CONN"
  runStdoutLoggingT $ withPostgresqlPool (toS connStr) 10 $ \pool ->
    control $ \runInIO -> do
      runResourceT $ flip runSqlPool pool $ runMigration migrateAll
      app <- makeFoundation pool
      tids <- runInIO $ Atman.startWorkers app
      end <- newEmptyMVar
      installHandler keyboardSignal (Catch $ putMVar end ()) Nothing
      race_
        (takeMVar end >> mapM_ killThread tids) $
        startServer app
      putText ""
  where
        port = 5000
        startServer = warp port
