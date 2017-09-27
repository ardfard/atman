{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Atman.Prelude
import Yesod.Core

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidget [julius|
      function checkLoginState() {
        FB.getLoginStatus(function(response){
          console.log(response);
          console.log(FB.getAccessToken());
        })
      }

      window.fbAsyncInit = function() {
        FB.init({
          appId      : '1423758007705834',
          xfbml      : true,
          version    : 'v2.10'
        });
        FB.AppEvents.logPageView();
      };

      (function(d, s, id){
         var js, fjs = d.getElementsByTagName(s)[0];
         if (d.getElementById(id)) {return;}
         js = d.createElement(s); js.id = id;
         js.src = "//connect.facebook.net/en_US/sdk.js";
         fjs.parentNode.insertBefore(js, fjs);
       }(document, 'script', 'facebook-jssdk'));
    |]
    [whamlet|
        <div #fb-root> 
        <p> Hello,
        <p> This is your gateway to Atman
        <fb:login-button scope="publish_actions" onlogin="checkLoginState();">
    |]
