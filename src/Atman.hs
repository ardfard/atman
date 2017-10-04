{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Atman
    ( crawl
    , publish
    ) where

  
import Atman.Prelude
import Atman.Model
import Atman.Fetcher.Hackernews
import Foundation (App(..))
import Database.Persist.Sql ( ConnectionPool, getBy, insert, entityVal )
import Control.Monad.Trans.Resource (MonadBaseControl)
import Control.Monad.Logger (MonadLogger, logInfo)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Monad.Catch (MonadThrow)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Facebook as Fb
import  Facebook ((#=))
import Control.Monad.Trans.Resource (runResourceT)
import Utility (runAction)
import Data.Aeson (decodeStrict, Value, encode)

type Atman a = forall m . ( MonadBaseControl IO m
                          , MonadReader App m
                          , MonadIO m
                          , MonadLogger m 
                          , MonadThrow m
                          , MonadError Text m)
                          => m a

crawl ::  Atman Item
crawl = do 
  $(logInfo) "start crawling"
  contents <- liftIO $ getHNContents 
  process contents
  where 
    process (x:xs) = do 
      ifM (checkItemExist $ x)
          (process xs)
          (return x)
    process _ = throwError "No item available"
    checkItemExist i = do 
      runDb $ do 
        mItem <- getBy . Digest $ itemDigest i
        case mItem of 
          Nothing -> do 
                      insert i
                      return False 
          _ -> return True

publish ::  Item -> Atman ()
publish (Item url desc _)  = do

  etoken <- runExceptT $ do 
     ExceptT $ do 
        eUser <- maybeToRight "No User Found"  <$> getAtmanUser
        return $ do user <- entityVal <$> eUser   
                    bsToken <- maybeToRight "No access token found" . userFacebookAccessToken $ user
                    maybeToRight "decode access failed" $ decodeToken bsToken

  case etoken of
    Right token -> do 
      App{..} <- ask
      let fbCred = Fb.Credentials { appName = fbAppName
                                  , appId = fbAppId
                                  , appSecret = fbAppSecret }
      mgr <- liftIO $ newManager tlsManagerSettings
      val :: Value <- runResourceT $ Fb.runFacebookT fbCred mgr $ postToFeed token
      $(logInfo) (show val)
    Left e -> throwError e

  where getAtmanUser = runDb $ getBy (Username "atman_user")
        decodeToken :: ByteString -> Maybe (Fb.UserAccessToken)
        decodeToken = decodeStrict 
        arguments = ["message" #= desc, "link" #= url]
        postToFeed token = Fb.postObject "/me/feed" arguments token
