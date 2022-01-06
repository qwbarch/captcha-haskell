{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.CapMonster.Internal.Types.ReCaptchaV2 where

import Captcha.CapMonster.Internal (CapMonster)
import Captcha.Internal (getProxyAddress, getProxyPassword, getProxyPort, getProxyType, getProxyUsername, renderCookies)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasDataS (dataS), HasProxy (proxy), HasUserAgent (userAgent), ReCaptchaV2)
import Control.Lens (preview, (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text, toLower)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster ReCaptchaV2 r m where
  request captcha = flip (post defaults) payload
    where
      payload =
        [aesonQQ|
          { 
            clientKey: #{captcha ^. apiKey},
            task: {
              type: #{("NoCaptchaTask" :: Text) <> maybe "Proxyless" (const mempty) (captcha ^. proxy)},
              websiteURL: #{captcha ^. captchaUrl},
              websiteKey: #{captcha ^. captchaKey},
              recaptchaDataSValue: #{captcha ^. dataS},
              userAgent: #{captcha ^. userAgent},
              proxyType: #{toLower <$> getProxyType captcha},
              proxyAddress: #{getProxyAddress captcha},
              proxyPort: #{getProxyPort captcha},
              proxyLogin: #{getProxyUsername captcha},
              proxyPassword: #{getProxyPassword captcha},
              cookies: #{renderCookies captcha}
            }
          }
        |]

instance CaptchaResponse CapMonster ReCaptchaV2 where
  parseResult = preview $ key "solution" . key "gRecaptchaResponse"
