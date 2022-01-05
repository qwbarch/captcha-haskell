{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.CapMonster.Internal.Types.HCaptcha where

import Captcha.CapMonster.Internal.CapMonster (CapMonster)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasInvisible (invisible), HasProxy (proxy), HasRqData (rqData), HasUserAgent (userAgent), getProxyAddress, getProxyPassword, getProxyPort, getProxyType, getProxyUsername, renderCookies)
import Control.Lens (preview, (^.))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text, toLower)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster HCaptcha r m where
  request captcha = flip (post defaults) payload
    where
      payload =
        [aesonQQ|
          {
            clientKey: #{captcha ^. apiKey},
            task: {
              type: #{("HCaptchaTask" :: Text) <> maybe "Proxyless" (const mempty) (captcha ^. proxy)},
              websiteURL: #{captcha ^. captchaUrl},
              websiteKey: #{captcha ^. captchaKey},
              isInvisible: #{show $ captcha ^. invisible},
              data: #{captcha ^. rqData},
              proxyType: #{toLower <$> getProxyType captcha},
              proxyAddress: #{getProxyAddress captcha},
              proxyPort: #{getProxyPort captcha},
              proxyLogin: #{getProxyUsername captcha},
              proxyPassword: #{getProxyPassword captcha},
              cookies: #{renderCookies captcha},
              userAgent: #{captcha ^. userAgent}
            }
          }
        |]

instance CaptchaResponse CapMonster HCaptcha where
  parseResult = preview $ key "solution" . key "gRecaptchaResponse"
