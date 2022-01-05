{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.TwoCaptcha.Test.HCaptcha where

import Captcha.Internal.Types (HCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl))
import Captcha.TwoCaptcha (ApiKey, assertCaptcha)
import Captcha.TwoCaptcha.Internal.Types.HCaptcha ()
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test key =
  testCase "HCaptcha" $
    let captcha =
          def @HCaptcha
            & apiKey .~ key
            & captchaUrl .~ "https://2captcha.com/demo/hcaptcha"
            & captchaKey .~ "3ceb8624-1970-4e6b-91d5-70317b70b651"
     in assertCaptcha captcha
