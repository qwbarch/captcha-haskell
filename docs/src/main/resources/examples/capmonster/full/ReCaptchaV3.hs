{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasAction (action), HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasCookies (cookies), HasMinScore (minScore), HasPollingInterval (pollingInterval), HasProxy (proxy), HasTimeoutDuration (timeoutDuration), HasUserAgent (userAgent), MonadCaptcha (solve), Proxy (Proxy), ProxyAuth (ProxyAuth), ProxyProtocol (Http), ReCaptchaV3, mkCaptchaEnv)
import Captcha.CapMonster (CapMonster)
import Control.Exception (throw)
import Control.Lens ((&), (.~), (?~))
import Data.Default (Default (def))
import Time (Time (Time))

main :: IO ()
main = do
  let captcha =
        def @ReCaptchaV3
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v3"
          & captchaKey .~ "6LfB5_IbAAAAAMCtsjEHEHKqcB9iQocwwxTiihJu"
          & minScore .~ 0.1
          & cookies .~ [("cookie_key", "cookie_value")]
          & action ?~ "demo action"
          & userAgent ?~ "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1"
          & proxy ?~ Proxy "proxy_address" Http 1234 (Just $ ProxyAuth "username" "password")
          & pollingInterval ?~ Time 5000
          & timeoutDuration ?~ Time 60_000
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @CapMonster captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
