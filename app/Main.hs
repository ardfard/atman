import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Atman.Prelude
import Atman.Model
import Atman.Crawler (crawl)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN, logDebugN)
import           Database.Persist.Postgresql
import System.Environment (lookupEnv)
import System.Cron (execSchedule, addJob)
import System.Posix.Signals

main :: IO ()
main = do 
  connStr <- maybe "host=localhost dbname=test user=test password=test port=5432" identity <$> lookupEnv "DB_CONN" 
  runStdoutLoggingT $ withPostgresqlPool (toS connStr) 10 $ \pool -> do 
    liftIO $ do
      runResourceT $ flip runSqlPool pool $ do
          runMigration migrateAll
      tids <- execSchedule $ addJob (crawlAndPost pool) "* * * * *"
      end  <- newEmptyMVar
      installHandler keyboardSignal (putMVar end ()) Nothing
      race_ 
       (do 
         takeMVar end
         mapM_ killThread tids )
       (warp 3000 $ App  pool)
  where crawlAndPost pool = do
          item <- runStdoutLoggingT . runResourceT  $ runReaderT  crawl  pool
          print item


