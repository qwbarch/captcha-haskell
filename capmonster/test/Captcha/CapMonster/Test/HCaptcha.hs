{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster.Test.HCaptcha where

import Captcha.CapMonster (ApiKey, assertCaptcha)
import Captcha.CapMonster.Internal.Types.HCaptcha ()
import Captcha.Internal.Types (HCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl))
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
