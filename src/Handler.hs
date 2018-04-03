{-# LANGUAGE QuasiQuotes #-}
module Handler where

import           Atman.Model
import           Atman.Prelude
import           Control.Monad.Logger      (logInfoN)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Aeson                (encode)
import           Database.Persist
import qualified Facebook                  as Fb
import           Foundation
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types        (status201)
import           Network.Wai               (requestHeaderHost)
import qualified Web.Telegram.API.Bot      as Telegram
import           Yesod.Core
import           Yesod.Persist.Core

fbRedirectUrl ∷ Handler Text
fbRedirectUrl = do
  render <- getUrlRender
  return $ render FbRedirectUrl

getFBToken ∷ Text → Handler ByteString
getFBToken code = do
  redirectUrl <- fbRedirectUrl
  toS . encode <$> runFacebookAction  (getAccessToken redirectUrl (toS code))


getFbRedirectUrl ∷ Handler ()
getFbRedirectUrl = do
  mToken <- runMaybeT $ MaybeT (lookupGetParam "code") >>= lift . getFBToken
  runDB $ updateWhere [UserUsername ==. "atman_user"] [UserFacebookAccessToken =. mToken ]
  redirect HomeR

runFacebookAction ∷ Fb.FacebookT Fb.Auth Handler a → Handler a
runFacebookAction action = do
  App{..} <- getYesod
  let fbCred = Fb.Credentials { appName = fbAppName
                              , appId = fbAppId
                              , appSecret = fbAppSecret }
  mgr <- liftIO $ newManager tlsManagerSettings
  Fb.runFacebookT fbCred mgr action

getHomeR ∷ Handler Html
getHomeR = do
  mUser <- runDB . getBy $ Username "atman_user"
  redirectUrl <- fbRedirectUrl
  fbLoginUrl <- runFacebookAction $ Fb.getUserAccessTokenStep1 redirectUrl ["publish_actions"]
  let logged = isJust (mUser >>= userFacebookAccessToken . entityVal )
  defaultLayout $
    [whamlet|
        <div #fb-root>
        <p> Hello,
        <p> This is your gateway to Atman
        $if not logged
          <a href=#{fbLoginUrl}> login
    |]

getAccessToken redirectUrl t = Fb.getUserAccessTokenStep2 redirectUrl [("code", t)]

patchUserR ∷ Handler ()
patchUserR = undefined

postTelegramWebhooks ∷ Handler ()
postTelegramWebhooks = do
  update <- requireJsonBody :: Handler Telegram.Update
  liftIO . print $ update
