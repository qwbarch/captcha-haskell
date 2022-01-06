{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasApiKey (apiKey), HasBody (body), ImageCaptcha, MonadCaptcha (solve), mkCaptchaEnv)
import Captcha.CapMonster (CapMonster)
import Control.Exception (throw)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))

main :: IO ()
main = do
  let captcha =
        def @ImageCaptcha
          & apiKey .~ "YOUR_API_KEY"
          & body .~ "BASE_64_ENCODED_IMAGE"
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @CapMonster captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
