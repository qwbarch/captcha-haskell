{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Captcha.CapMonster.Internal.Types.FunCaptcha
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.CapMonster.Internal.Types.FunCaptcha where

import Captcha.CapMonster.Internal (CapMonster)
import Captcha.Internal (getProxyAddress, getProxyPassword, getProxyPort, getProxyType, getProxyUsername, renderCookies)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (FunCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasProxy (proxy), HasServiceUrl (serviceUrl), HasUserAgent (userAgent))
import Control.Lens (preview, (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text, toLower)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster FunCaptcha r m where
  request captcha = flip (post defaults) payload
    where
      payload =
        [aesonQQ|
          { 
            clientKey: #{captcha ^. apiKey},
            task: {
              type: #{("FunCaptchaTask" :: Text) <> maybe "Proxyless" (const mempty) (captcha ^. proxy)},
              websiteURL: #{captcha ^. captchaUrl},
              websitePublicKey: #{captcha ^. captchaKey},
              funcaptchaApiJSSubdomain: #{captcha ^. serviceUrl},
              userAgent: #{captcha ^. userAgent},
              proxyType: #{toLower <$> getProxyType captcha},
              proxyAddress: #{getProxyAddress captcha},
              proxyPort: #{getProxyPort captcha},
              proxyLogin: #{getProxyUsername captcha},
              proxyPassword: #{getProxyPassword captcha},
              cookies: #{renderCookies captcha}
            }
          } |]

instance CaptchaResponse CapMonster FunCaptcha where
  parseResult = preview $ key "solution" . key "token"
