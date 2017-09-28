import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Atman.Prelude
import Atman.Model
import Atman.Crawler (crawl)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Control (control)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN, logDebugN)
import           Database.Persist.Postgresql
import System.Environment (lookupEnv)
import System.Cron (execSchedule, addJob)
import System.Posix.Signals

main :: IO ()
main = do 
  connStr <- maybe "host=localhost dbname=test user=test password=test port=5432" identity <$> lookupEnv "DB_CONN" 
  runStdoutLoggingT $ withPostgresqlPool (toS connStr) 10 $ \pool -> do 
    control $ \runInIO ->  do
      runResourceT $ flip runSqlPool pool $ do
          runMigration migrateAll
      runInIO $ logInfoN "Initializing crawlers"
      print "Starting program" 
      publishChan <- newChan
      tids <- execSchedule $ addJob (runInIO $ crawlAndPost publishChan pool) "* * * * *"
      runInIO $ logInfoN "starting web server on port 3000"
      end <- newEmptyMVar
      installHandler keyboardSignal (Catch $ putMVar end ()) Nothing
      race_ 
       (do 
         takeMVar end
         mapM_ killThread tids )
       ( concurrently_ (publish publishChan) $ startServer pool )
  where crawlAndPost  c pool = liftIO . writeChan c  =<< runResourceT  ( runReaderT  crawl  pool )
        startServer pool = warp 3000 $ App pool
        publish c = void . forever $ print =<< readChan c



