{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster.Test.ReCaptchaV2 where

import Captcha.CapMonster (ApiKey, assertCaptcha)
import Captcha.CapMonster.Internal.Types.ReCaptchaV2 ()
import Captcha.Internal.Types (HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), ReCaptchaV2)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test key =
  testCase "ReCaptchaV2" $
    let captcha =
          def @ReCaptchaV2
            & apiKey .~ key
            & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v2"
            & captchaKey .~ "6LfD3PIbAAAAAJs_eEHvoOl75_83eXSqpPSRFJ_u"
     in assertCaptcha captcha
