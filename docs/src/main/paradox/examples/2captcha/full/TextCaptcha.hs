{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasApiKey (apiKey), HasBody (body), HasPollingInterval (pollingInterval), HasTimeoutDuration (timeoutDuration), MonadCaptcha (solve), TextCaptcha, mkCaptchaEnv)
import Captcha.TwoCaptcha (TwoCaptcha)
import Control.Exception (throw)
import Control.Lens ((&), (.~), (?~))
import Data.Default (Default (def))
import Time (Time (Time))

main :: IO ()
main = do
  let captcha =
        def @TextCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & body .~ "YOUR_TEXT_CAPTCHA"
          & pollingInterval ?~ Time 5000
          & timeoutDuration ?~ Time 60_000
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @TwoCaptcha captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
