{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Atman.Prelude
import Yesod.Core
import Yesod.Persist.Core
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlBackend)
import Database.Persist

data App = App { connPool :: ConnectionPool }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App 

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    app <- getYesod
    runSqlPool action $ connPool app

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner connPool