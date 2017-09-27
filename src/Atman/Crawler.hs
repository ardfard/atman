{-# LANGUAGE TemplateHaskell   #-}

module Atman.Crawler where 

  
import Atman.Prelude
import Atman.Model
import Atman.Fetcher.Hackernews
import Database.Persist.Sql ( ConnectionPool, getBy, insert )
import Control.Monad.Trans.Resource (MonadBaseControl)
import Control.Monad.Logger (MonadLogger, logInfo)

crawl :: ( MonadBaseControl IO m
         , MonadReader ConnectionPool m
         , MonadIO m 
         , MonadLogger m
         ) => m (Maybe Item)
crawl = do 
  $(logInfo) "start crawling"
  contents <- liftIO $ getHNContents 
  process contents
  where 
    process (x:xs) = do 
      ifM (checkItemExist $ x)
          (process xs)
          (return $ Just x)
    process _ = return Nothing
    checkItemExist i = do 
      runDb $ do 
        mItem <- getBy . Digest $ itemDigest i
        case mItem of 
          Nothing -> do 
                      insert i
                      return False 
          _ -> return True

