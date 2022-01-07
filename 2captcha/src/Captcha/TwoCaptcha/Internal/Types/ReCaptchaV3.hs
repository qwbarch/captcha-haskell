{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Captcha.TwoCaptcha.Internal.Types.ReCaptchaV3
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.TwoCaptcha.Internal.Types.ReCaptchaV3 where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasMinScore (minScore), ReCaptchaV3)
import Captcha.TwoCaptcha.Internal (TwoCaptcha, defaultOptions)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (object)
import Data.String.Conversions (cs)
import Network.Wreq (param)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest TwoCaptcha ReCaptchaV3 r m where
  request captcha = flip (post options) (object [])
    where
      options =
        defaultOptions
          & param "key" .~ [captcha ^. apiKey]
          & param "method" .~ ["userrecaptcha"]
          & param "version" .~ ["v3"]
          & param "googlekey" .~ [captcha ^. captchaKey]
          & param "pageurl" .~ [captcha ^. captchaUrl]
          & param "min_score" .~ [cs . show $ captcha ^. minScore]
