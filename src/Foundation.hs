{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Atman.Prelude
import Yesod.Core
import Yesod.Persist.Core
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlBackend)
import Database.Persist

data App = App { connPool :: ConnectionPool
               , fbAppName :: Text
               , fbAppId :: Text
               , fbAppSecret :: Text
               }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  approot = ApprootRequest $ \app req ->
        getApprootText guessApproot app req

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    app <- getYesod
    runSqlPool action $ connPool app

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner connPool