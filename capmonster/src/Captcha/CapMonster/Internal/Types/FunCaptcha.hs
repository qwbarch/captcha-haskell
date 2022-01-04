{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Captcha.CapMonster.Internal.Types.FunCaptcha where

import Captcha.CapMonster.Internal.CapMonster (CapMonster, createTaskUrl)
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest (request), CaptchaResponse (parseResult))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (FunCaptcha, HasAddress (address), HasApiKey (apiKey), HasAuth (auth), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasPassword (password), HasPort (port), HasProtocol (protocol), HasProxy (proxy), HasServiceUrl (serviceUrl), HasUserAgent (userAgent), HasUsername (username), renderCookies)
import Control.Lens (preview, (^.), (^?), _Just)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.Lens (key)
import Data.Aeson.QQ (aesonQQ)
import Data.Char (toLower)
import Data.Text (Text)
import Network.Wreq (defaults)

instance (HasCaptchaEnv r, MonadReader r m, MonadIO m) => CaptchaRequest CapMonster FunCaptcha r m where
  request captcha = post defaults createTaskUrl payload
    where
      fromProxy x = captcha ^? proxy . _Just . x
      payload =
        [aesonQQ|
          { 
            clientKey: #{captcha ^. apiKey},
            task: {
              type: #{("FunCaptchaTask" :: Text) <> maybe "Proxyless" (const mempty) (captcha ^. proxy)},
              websiteURL: #{captcha ^. captchaUrl},
              websitePublicKey: #{captcha ^. captchaKey},
              funcaptchaApiJSSubdomain: #{captcha ^. serviceUrl},
              userAgent: #{captcha ^. userAgent},
              proxyType: #{fmap toLower . show <$> fromProxy protocol},
              proxyAddress: #{fromProxy address},
              proxyPort: #{fromProxy port},
              proxyLogin: #{fromProxy $ auth . _Just . username},
              proxyPassword: #{fromProxy $ auth . _Just . password},
              cookies: #{renderCookies captcha}
            }
          }
        |]

instance CaptchaResponse CapMonster FunCaptcha where
  parseResult = preview $ key "solution" . key "token"
