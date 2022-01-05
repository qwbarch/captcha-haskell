{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.TwoCaptcha.Internal.Types.ReCaptchaV2 where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasCookies (cookies), HasDataS (dataS), HasInvisible (invisible), HasUserAgent (userAgent), ReCaptchaV2, renderCookies)
import Captcha.TwoCaptcha.Internal.TwoCaptcha (TwoCaptcha, parseProxy, parseProxyType)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (object)
import Data.Maybe (maybeToList)
import Data.String.Conversions (cs)
import Data.Text.Lazy (toStrict)
import Network.Wreq (defaults, param)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest TwoCaptcha ReCaptchaV2 r m where
  request captcha = flip (post options) (object [])
    where
      options =
        defaults
          & param "key" .~ [captcha ^. apiKey]
          & param "method" .~ ["userrecaptcha"]
          & param "googlekey" .~ [captcha ^. captchaKey]
          & param "pageurl" .~ [captcha ^. captchaUrl]
          & param "data-s" .~ maybeToList (captcha ^. dataS)
          & param "invisible" .~ [cs . show $ captcha ^. invisible]
          & param "userAgent" .~ maybeToList (captcha ^. userAgent)
          & param "cookies" .~ [toStrict $ renderCookies captcha]
          & param "proxy" .~ parseProxy captcha
          & param "proxytype" .~ parseProxyType captcha
          & param "json" .~ ["1"]
