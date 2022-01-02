{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.CapMonster.Internal.Types.Image where

import Captcha.CapMonster.Internal.CapMonster (CapMonster, createTaskUrl)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaCtx (request))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), ImageCaptcha)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaCtx (ImageCaptcha CapMonster) r m where
  request :: ImageCaptcha CapMonster -> m (Response ByteString)
  request captcha = do
    let payload =
          [aesonQQ|
            {
              clientKey: #{captcha ^. apiKey},
              task: {
                type: "ImageToTextTask",
                body: #{captcha ^. body}
              }
            }
          |]
    post defaults createTaskUrl payload
