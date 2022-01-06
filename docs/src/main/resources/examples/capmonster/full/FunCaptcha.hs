{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), FunCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasCookies (cookies), HasPollingInterval (pollingInterval), HasProxy (proxy), HasServiceUrl (serviceUrl), HasTimeoutDuration (timeoutDuration), HasUserAgent (userAgent), MonadCaptcha (solve), Proxy (Proxy), ProxyAuth (ProxyAuth), ProxyProtocol (Http), mkCaptchaEnv)
import Captcha.CapMonster (CapMonster)
import Control.Exception (throw)
import Control.Lens ((&), (.~), (?~))
import Data.Default (Default (def))
import Time (Time (Time))

main :: IO ()
main = do
  let captcha =
        def @FunCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://api.funcaptcha.com/fc/api/nojs"
          & captchaKey .~ "69A21A01-CC7B-B9C6-0F9A-E7FA06677FFC"
          & cookies .~ [("cookie_key", "cookie_value")]
          & serviceUrl ?~ "https://api.funcaptcha.com"
          & userAgent ?~ "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1"
          & proxy ?~ Proxy "proxy_address" Http 1234 (Just $ ProxyAuth "username" "password")
          & pollingInterval ?~ Time 5000
          & timeoutDuration ?~ Time 60_000
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @CapMonster captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
