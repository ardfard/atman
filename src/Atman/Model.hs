{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Atman.Model where

import Atman.Prelude
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql ( runSqlPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
  url Text
  description Text
  digest Text
  Digest digest
  deriving Show
|]

runDb action = do
                pool <- ask
                runSqlPool action pool
