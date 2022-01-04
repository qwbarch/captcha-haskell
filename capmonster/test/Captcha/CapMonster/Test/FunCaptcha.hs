{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster.Test.FunCaptcha where

import Captcha.CapMonster (ApiKey, assertCaptcha)
import Captcha.CapMonster.Internal.Types.FunCaptcha ()
import Captcha.Internal.Types (FunCaptcha, HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl))
import Control.Lens ((&), (.~))
import Control.Monad (void)
import Data.Default (Default (def))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test key = do
  testCase "FunCaptcha" $
    let captcha =
          def @FunCaptcha
            & apiKey .~ key
            & captchaUrl .~ "https://api.funcaptcha.com/fc/api/nojs/?pkey=69A21A01-CC7B-B9C6-0F9A-E7FA06677FFC"
            & captchaKey .~ "69A21A01-CC7B-B9C6-0F9A-E7FA06677FFC"
     in void $ assertCaptcha captcha (const $ pure ())
