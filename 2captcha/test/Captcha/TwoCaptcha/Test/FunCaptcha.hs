{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.TwoCaptcha.Test.FunCaptcha where

import Captcha.Internal.Types (FunCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl))
import Captcha.TwoCaptcha (ApiKey, assertCaptcha)
import Captcha.TwoCaptcha.Internal.Types.FunCaptcha ()
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test key = do
  testCase "FunCaptcha" $
    let captcha =
          def @FunCaptcha
            & apiKey .~ key
            & captchaUrl .~ "https://api.funcaptcha.com/fc/api/nojs"
            & captchaKey .~ "69A21A01-CC7B-B9C6-0F9A-E7FA06677FFC"
     in assertCaptcha captcha
