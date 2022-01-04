{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.CapMonster.Internal.Types.Image where

import Captcha.CapMonster.Internal.CapMonster (CapMonster, createTaskUrl)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), ImageCaptcha)
import Control.Lens (preview, (^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster ImageCaptcha r m where
  request captcha = post defaults createTaskUrl payload
    where
      payload =
        [aesonQQ|
          {
            clientKey: #{captcha ^. apiKey},
            task: {
              type: "ImageToTextTask",
              body: #{captcha ^. body}
            }
          }
        |]

instance CaptchaResponse CapMonster ImageCaptcha where
  parseResult = preview $ key "solution" . key "text"
