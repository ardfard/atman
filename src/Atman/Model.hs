{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}

module Atman.Model where

import Atman.Prelude
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql ( runSqlPool)
import Yesod.Core.Json (ToJSON, FromJSON)
import Foundation (App(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
  url Text
  description Text
  digest Text
  Digest digest
  deriving Show
User
  username Text
  facebookAccessToken ByteString Maybe
  Username username
  deriving Show
|]

runDb action = do
                pool <- asks connPool
                runSqlPool action pool
