{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.TwoCaptcha.Internal.Types.FunCaptcha where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (FunCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasServiceUrl (serviceUrl), HasUserAgent (userAgent))
import Captcha.TwoCaptcha.Internal (TwoCaptcha, defaultOptions, parseProxy, parseProxyType)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (object)
import Data.Maybe (maybeToList)
import Network.Wreq (param)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest TwoCaptcha FunCaptcha r m where
  request captcha = flip (post options) (object [])
    where
      options =
        defaultOptions
          & param "key" .~ [captcha ^. apiKey]
          & param "method" .~ ["funcaptcha"]
          & param "publickey" .~ [captcha ^. captchaKey]
          & param "pageurl" .~ [captcha ^. captchaUrl]
          & param "surl" .~ maybeToList (captcha ^. serviceUrl)
          & param "userAgent" .~ maybeToList (captcha ^. userAgent)
          & param "proxy" .~ parseProxy captcha
          & param "proxytype" .~ parseProxyType captcha
