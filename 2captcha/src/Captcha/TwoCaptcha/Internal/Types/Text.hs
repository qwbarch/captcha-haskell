{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Captcha.TwoCaptcha.Internal.Types.Text
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.TwoCaptcha.Internal.Types.Text where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), TextCaptcha)
import Captcha.TwoCaptcha.Internal (TwoCaptcha, defaultOptions)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (object)
import Network.Wreq (param)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest TwoCaptcha TextCaptcha r m where
  request captcha = flip (post options) (object [])
    where
      options =
        defaultOptions
          & param "key" .~ [captcha ^. apiKey]
          & param "textcaptcha" .~ [captcha ^. body]
