{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), FunCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), MonadCaptcha (solve), mkCaptchaEnv)
import Captcha.CapMonster (CapMonster)
import Control.Exception (throw)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))

main :: IO ()
main = do
  let captcha =
        def @FunCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://api.funcaptcha.com/fc/api/nojs"
          & captchaKey .~ "69A21A01-CC7B-B9C6-0F9A-E7FA06677FFC"
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @CapMonster captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
