{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Captcha.CapMonster.Internal.Types.ReCaptchaV3
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.CapMonster.Internal.Types.ReCaptchaV3 where

import Captcha.CapMonster.Internal (CapMonster)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasAction (action), HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasMinScore (minScore), ReCaptchaV3)
import Control.Lens (preview, (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster ReCaptchaV3 r m where
  request captcha = flip (post defaults) payload
    where
      payload =
        [aesonQQ|
          {
            clientKey: #{captcha ^. apiKey},
            task: {
              type: "RecaptchaV3TaskProxyless",
              websiteURL: #{captcha ^. captchaUrl},
              websiteKey: #{captcha ^. captchaKey},
              minScore: #{captcha ^. minScore},
              pageAction: #{captcha ^. action}
            }
          }
        |]

instance CaptchaResponse CapMonster ReCaptchaV3 where
  parseResult = preview $ key "solution" . key "gRecaptchaResponse"
