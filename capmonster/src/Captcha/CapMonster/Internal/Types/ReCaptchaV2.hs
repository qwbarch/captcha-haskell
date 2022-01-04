{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.CapMonster.Internal.Types.ReCaptchaV2 where

import Captcha.CapMonster.Internal.CapMonster (CapMonster, createTaskUrl)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasAddress (address), HasApiKey (apiKey), HasAuth (auth), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasDataS (dataS), HasPassword (password), HasPort (port), HasProtocol (protocol), HasProxy (proxy), HasUserAgent (userAgent), HasUsername (username), ReCaptchaV2, renderCookies)
import Control.Lens (preview, (^.), (^?), _Just)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Data.Char (toLower)
import Data.Text (Text)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster ReCaptchaV2 r m where
  request captcha = post defaults createTaskUrl payload
    where
      fromProxy x = captcha ^? proxy . _Just . x
      payload =
        [aesonQQ|
          { 
            clientKey: #{captcha ^. apiKey},
            task: {
              type: #{"NoCaptchaTask" <> maybe ("Proxyless" :: Text) (const mempty) (captcha ^. proxy)},
              websiteURL: #{captcha ^. captchaUrl},
              websiteKey: #{captcha ^. captchaKey},
              recaptchaDataSValue: #{captcha ^. dataS},
              userAgent: #{captcha ^. userAgent},
              proxyType: #{toLower <$> (show $ preview (proxy . _Just . protocol) captcha)},
              proxyAddress: #{fromProxy address},
              proxyPort: #{fromProxy port},
              proxyLogin: #{fromProxy $ auth . _Just . username},
              proxyPassword: #{fromProxy $ auth . _Just . password},
              cookies: #{renderCookies captcha}
            }
          }
        |]

instance CaptchaResponse CapMonster ReCaptchaV2 where
  parseResult = preview $ key "solution" . key "gRecaptchaResponse"
