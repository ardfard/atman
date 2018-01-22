{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Atman
    ( start
    , publish
    , crawl
    ) where


import           Atman.Fetcher.Hackernews
import           Atman.Model
import           Atman.Prelude
import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.Logger         (LoggingT, MonadLogger, logErrorN,
                                               logInfo)
import           Control.Monad.Trans.Control  (control)
import           Control.Monad.Trans.Except   (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Resource (MonadBaseControl, runResourceT)
import           Data.Aeson                   (Value, decodeStrict, encode)
import           Database.Persist.Sql         (entityVal, getBy, insert,
                                               insertBy)
import           Facebook                     (( #= ))
import qualified Facebook                     as Fb
import           Foundation                   (App (..))
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Cron                  (addJob, execSchedule)
import           Utility                      (runAction)

type Atman a = forall m . ( MonadBaseControl IO m
                          , MonadReader App m
                          , MonadIO m
                          , MonadLogger m
                          , MonadThrow m
                          , MonadError Text m)
                          ⇒ m a

crawl ∷  Atman Item
crawl = do
  $(logInfo) "start crawling"
  contents <- liftIO getHNContents
  process contents
  where
    process (x:xs) =
      ifM (checkItemExist x)
          (process xs)
          (return x)
    process _ = throwError "No item available"
    checkItemExist i =
      runDb $ do
        mItem <- getBy . Digest $ itemDigest i
        case mItem of
          Nothing -> do
                      insert i
                      return False
          _ -> return True

publish ∷  Item → Atman ()
publish (Item url desc _)  = do

  etoken <- runExceptT $
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
        decodeToken ∷ ByteString → Maybe Fb.UserAccessToken
        decodeToken = decodeStrict
        arguments = ["message" #= desc, "link" #= url]
        postToFeed = Fb.postObject "/me/feed" arguments

init ∷ Atman ()
init = void . runDb $ insertBy (User "atman_user" Nothing)

start ∷ App → LoggingT IO [ThreadId]
start app = do
  runAtman app init
  control $ \runInIO ->  do
    chan <- newChan
    tids <- execSchedule $ addJob (runInIO $ crawlWorker chan ) "0 */8 * * *"
    tid <- forkIO . runInIO $ publishWorker chan
    return $ tid:tids
  where
    runAtman app = runExceptT . flip runReaderT app . runResourceT
    publishWorker c = forever $ do
      item  <- liftIO $ readChan c
      eres <-  runAtman app $ publish item
      $(logInfo) $ "Published :" <> show item
    crawlWorker c = do
      eitem <- runAtman app crawl
      case eitem of
        Right item -> liftIO $ writeChan c item
        Left e     -> logErrorN e
