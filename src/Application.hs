{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import           Atman.Prelude
import           Database.Persist.Sql (ConnectionPool)
import           Foundation
import           System.Environment   (lookupEnv)
import           Yesod.Core

import           Handler

mkYesodDispatch "App" resourcesApp

makeFoundation :: (MonadIO m) => ConnectionPool -> m App
makeFoundation pool = liftIO $ do
  connStr <- fromMaybe "host=localhost dbname=atman user=atman password=testing port=5432" <$> lookupEnv "DB_CONN"
  id_ <- fromMaybe "" <$> lookupEnv "FB_APP_ID"
  name <- fromMaybe "" <$> lookupEnv "FB_APP_NAME"
  secret <- fromMaybe "" <$> lookupEnv "FB_APP_SECRET"
  chatId <- fromMaybe "" <$> lookupEnv "TELEGRAM_CHAT_ID"
  token <- fromMaybe "" <$> lookupEnv "TELEGRAM_TOKEN"
  return $ App pool (toS name) (toS id_) (toS secret) (fromMaybe 0 . readMaybe $ chatId) (toS token)
