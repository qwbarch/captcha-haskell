{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasApiKey (apiKey), HasBody (body), MonadCaptcha (solve), TextCaptcha, mkCaptchaEnv)
import Captcha.TwoCaptcha (TwoCaptcha)
import Control.Exception (throw)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))

main :: IO ()
main = do
  let captcha =
        def @TextCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & body .~ "YOUR_TEXT_CAPTCHA"
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @TwoCaptcha captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
