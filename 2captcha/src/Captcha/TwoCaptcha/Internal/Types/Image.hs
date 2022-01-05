{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.TwoCaptcha.Internal.Types.Image where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), ImageCaptcha)
import Captcha.TwoCaptcha.Internal.TwoCaptcha (TwoCaptcha, defaultOptions)
import Control.Lens ((^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)
import Network.Wreq (FormParam ((:=)))

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest TwoCaptcha ImageCaptcha r m where
  request captcha = flip (post defaultOptions) payload
    where
      payload =
        [ "key" := (captcha ^. apiKey),
          "method" := ("base64" :: Text),
          "body" := (captcha ^. body)
        ]
