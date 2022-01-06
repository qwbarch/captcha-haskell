{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasCookies (cookies), HasInvisible (invisible), HasPollingInterval (pollingInterval), HasProxy (proxy), HasRqData (rqData), HasTimeoutDuration (timeoutDuration), HasUserAgent (userAgent), MonadCaptcha (solve), Proxy (Proxy), ProxyAuth (ProxyAuth), ProxyProtocol (Http), mkCaptchaEnv)
import Captcha.TwoCaptcha (TwoCaptcha)
import Control.Exception (throw)
import Control.Lens ((&), (.~), (?~))
import Data.Default (Default (def))
import Time (Time (Time))

main :: IO ()
main = do
  let captcha =
        def @HCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://2captcha.com/demo/hcaptcha"
          & captchaKey .~ "3ceb8624-1970-4e6b-91d5-70317b70b651"
          & invisible .~ False
          & cookies .~ [("cookie_key", "cookie_value")]
          & rqData ?~ "CUSTOM_HCAPTCHA_DATA"
          & userAgent ?~ "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1"
          & proxy ?~ Proxy "proxy_address" Http 1234 (Just $ ProxyAuth "username" "password")
          & pollingInterval ?~ Time 5000
          & timeoutDuration ?~ Time 60_000
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @TwoCaptcha captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
