{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), HasCookies (cookies), HasDataS (dataS), HasInvisible (invisible), HasPollingInterval (pollingInterval), HasProxy (proxy), HasTimeoutDuration (timeoutDuration), HasUserAgent (userAgent), MonadCaptcha (solve), Proxy (Proxy), ProxyAuth (ProxyAuth), ProxyProtocol (Http), ReCaptchaV2, mkCaptchaEnv)
import Captcha.CapMonster (CapMonster)
import Control.Exception (throw)
import Control.Lens ((&), (.~), (?~))
import Data.Default (Default (def))
import Time (Time (Time))

main :: IO ()
main = do
  let captcha =
        def @ReCaptchaV2
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v2-invisible"
          & captchaKey .~ "6LfDxboZAAAAAD6GHukjvUy6lszoeG3H4nQW57b6"
          & invisible .~ True
          & cookies .~ [("cookie_key", "cookie_value")]
          & dataS ?~ "RECAPTCHA_DATA_S_VALUE"
          & userAgent ?~ "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1"
          & proxy ?~ Proxy "proxy_address" Http 1234 (Just $ ProxyAuth "username" "password")
          & pollingInterval ?~ Time 5000
          & timeoutDuration ?~ Time 60_000
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @CapMonster captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
