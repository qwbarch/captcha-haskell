{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), MonadCaptcha (solve), mkCaptchaEnv)
import Captcha.CapMonster (CapMonster)
import Control.Exception (throw)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))

main :: IO ()
main = do
  let captcha =
        def @HCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://2captcha.com/demo/hcaptcha"
          & captchaKey .~ "3ceb8624-1970-4e6b-91d5-70317b70b651"
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @CapMonster captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
