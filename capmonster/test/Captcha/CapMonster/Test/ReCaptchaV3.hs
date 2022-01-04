{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster.Test.ReCaptchaV3 where

import Captcha.CapMonster (ApiKey, assertCaptcha)
import Captcha.CapMonster.Internal.Types.ReCaptchaV3 ()
import Captcha.Internal.Types (HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), ReCaptchaV3)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test _apiKey =
  testCase "ReCaptchaV3" $
    let captcha =
          def @ReCaptchaV3
            & apiKey .~ _apiKey
            & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v3"
            & captchaKey .~ "6LfB5_IbAAAAAMCtsjEHEHKqcB9iQocwwxTiihJu"
     in assertCaptcha captcha
