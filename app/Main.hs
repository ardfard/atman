import Application (makeFoundation) -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Atman.Prelude
import Atman.Model
import Atman (crawl, publish)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Control (control)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN, logErrorN, logDebugN)
import           Database.Persist.Postgresql
import System.Cron (execSchedule, addJob)
import System.Environment (lookupEnv)
import System.Posix.Signals

main :: IO ()
main = do
  connStr <- maybe "host=localhost dbname=atman user=atman password=testing port=5432" identity <$> lookupEnv "DB_CONN" 
  runStdoutLoggingT $ withPostgresqlPool (toS connStr) 10 $ \pool -> do 
    control $ \runInIO ->  do
      runResourceT $ flip runSqlPool pool $ do
          runMigration migrateAll
      runInIO $ logInfoN "Initializing crawlers"
      print "Starting program" 
      publishChan <- newChan
      app <- makeFoundation pool
      tids <- execSchedule $ addJob (runInIO $ crawlWorker publishChan app) "0 */8 * * *"
      runInIO . logInfoN $ "starting web server on port " <> show port 
      end <- newEmptyMVar
      installHandler keyboardSignal (Catch $ putMVar end ()) Nothing
      race_ 
        (do 
         takeMVar end
         mapM_ killThread tids )
       ( concurrently_ (runInIO $ publishWorker publishChan app) $  startServer app )
  where crawlWorker c app = do 
          eitem <- runAtman app crawl
          either logErrorN (liftIO . writeChan c) eitem
        runAtman app action = runExceptT . runResourceT $ runReaderT action app
        port = 5000
        startServer = warp port 
        publishWorker c app = forever $ do 
          item  <- liftIO $ readChan c
          eres <- runAtman app $ publish item
          either logErrorN (const $ logInfoN $ "Published :" <> show item ) eres 
