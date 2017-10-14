{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Atman.Prelude
import Yesod.Core
import System.Environment (lookupEnv)
import Database.Persist.Sql (ConnectionPool)

import Handler

mkYesodDispatch "App" resourcesApp

makeFoundation :: (MonadIO m) => ConnectionPool -> m App
makeFoundation pool = liftIO $ do
  connStr <- fromMaybe "host=localhost dbname=atman user=atman password=testing port=5432" <$> lookupEnv "DB_CONN"
  id_ <- fromMaybe "" <$> lookupEnv "FB_APP_ID"
  name <- fromMaybe "" <$> lookupEnv "FB_APP_NAME"
  secret <- fromMaybe "" <$> lookupEnv "FB_APP_SECRET"
  return $ App pool (toS name) (toS id_) (toS secret)
