{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Atman.Prelude
import Yesod.Core

import Home

mkYesodDispatch "App" resourcesApp
