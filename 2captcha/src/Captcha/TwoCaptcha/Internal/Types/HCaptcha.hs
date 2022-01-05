{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.TwoCaptcha.Internal.Types.HCaptcha where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasInvisible (invisible), HasRqData (rqData), HasUserAgent (userAgent))
import Captcha.TwoCaptcha.Internal.TwoCaptcha (TwoCaptcha, defaultOptions, parseProxy, parseProxyType)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (object)
import Data.Maybe (maybeToList)
import Data.String.Conversions (cs)
import Network.Wreq (param)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest TwoCaptcha HCaptcha r m where
  request captcha = flip (post options) (object [])
    where
      options =
        defaultOptions
          & param "key" .~ [captcha ^. apiKey]
          & param "method" .~ ["hcaptcha"]
          & param "sitekey" .~ [captcha ^. captchaKey]
          & param "pageurl" .~ [captcha ^. captchaUrl]
          & param "invisible" .~ [cs . show $ captcha ^. invisible]
          & param "data" .~ maybeToList (captcha ^. rqData)
          & param "userAgent" .~ maybeToList (captcha ^. userAgent)
          & param "proxy" .~ parseProxy captcha
          & param "proxytype" .~ parseProxyType captcha
