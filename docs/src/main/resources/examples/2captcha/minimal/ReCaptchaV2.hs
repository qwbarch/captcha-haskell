{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), MonadCaptcha (solve), ReCaptchaV2, mkCaptchaEnv)
import Captcha.TwoCaptcha (TwoCaptcha)
import Control.Exception (throw)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))

main :: IO ()
main = do
  let captcha =
        def @ReCaptchaV2
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v2"
          & captchaKey .~ "6LeIxboZAAAAAFQy7d8GPzgRZu2bV0GwKS8ue_cH"
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @TwoCaptcha captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
