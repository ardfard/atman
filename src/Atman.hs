{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Atman
    ( startWorkers
    , crawl
    , sendTelegramMessage
    , facebookPublish
    , telegramPublish
    , Atman
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
import qualified Data.Text                    as T
import           Database.Persist.Sql         (entityVal, getBy, insert,
                                               insertBy)
import           Facebook                     (( #= ))
import qualified Facebook                     as Fb
import           Foundation                   (App (..))
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Cron                  (addJob, execSchedule)
import qualified Web.Telegram.API.Bot         as Telegram

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


sendTelegramMessage ∷ Telegram.ChatId -> Item → Telegram.TelegramClient ()
sendTelegramMessage chatId (Item url desc _) = do
  Telegram.sendMessageM $
    (Telegram.sendMessageRequest chatId message) {
        Telegram.message_parse_mode = Just Telegram.Markdown
    }
  return ()
  where
    message = T.concat [
        "*", desc, "*\n"
      , "[Article](", url,")"
      ]

telegramPublish ∷ Item → User → Atman ()
telegramPublish item user = do
  App{..} <- ask
  mgr <- liftIO $ newManager tlsManagerSettings
  liftIO $ Telegram.runTelegramClient (Telegram.Token $ "bot" <> telegramToken) mgr (sendTelegramMessage (Telegram.ChatId telegramChatId) item)
  return ()

facebookPublish ∷ Item → User → Atman ()
facebookPublish (Item url desc _) user =
  case etoken of
    Right token -> do
      App{..} <- ask
      let fbCred = Fb.Credentials { appName = fbAppName
                                  , appId = fbAppId
                                  , appSecret = fbAppSecret }
      mgr <- liftIO $ newManager tlsManagerSettings
      val :: Value <- runResourceT . Fb.runFacebookT fbCred mgr $ postToFeed token
      $(logInfo) (show val)
    Left e -> throwError e
  where
    etoken = do
      bsToken <- maybeToRight "No access token found" . userFacebookAccessToken $ user
      maybeToRight "decode access failed" $ decodeToken bsToken
    decodeToken ∷ ByteString → Maybe Fb.UserAccessToken
    decodeToken = decodeStrict
    postToFeed = Fb.postObject "/me/feed" ["message" #= desc, "link" #= url]

init ∷ Atman ()
init = void . runDb $ insertBy (User "atman_user" Nothing)

startWorkers ∷ App → LoggingT IO [ThreadId]
startWorkers app = do
  runAtman app init
  control $ \runInIO ->  do
    chan <- newChan

    -- periodically crawl sources
    tids <- execSchedule $ addJob (runInIO $ crawlWorker chan ) cronSchedule
    euser <- runAtman app getAtmanUser
    case entityVal <$> euser of
      Left e -> runInIO (logErrorN e) >> return tids
      (Right user) -> do
        -- start publisher worker
        tid <- forkIO . forever $ do
            item <- readChan chan
            traverse_ (forkIO . runInIO . publish item user)  [facebookPublish, telegramPublish]
        return $ tid:tids
  where
    cronSchedule = "* * * * *"
    runAtman app = runExceptT . flip runReaderT app . runResourceT
    getAtmanUser = do
      muser <- runDb $ getBy (Username "atman_user")
      case muser of
        Nothing     -> throwError "User not found"
        (Just user) -> return user
    publish item user publisher = do
      eres <- runAtman app (publisher item user)
      case eres of
        Left e -> logErrorN e
        _      -> return ()
    crawlWorker c = do
      eitem <- runAtman app crawl
      case eitem of
        Right item -> liftIO $ writeChan c item
        Left e     -> logErrorN e
