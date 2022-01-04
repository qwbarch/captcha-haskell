{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster.Test.ReCaptchaV2 where

import Captcha.CapMonster (ApiKey, assertCaptcha)
import Captcha.CapMonster.Internal.Types.ReCaptchaV2 ()
import Captcha.Internal.Types (HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), ReCaptchaV2)
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad (void)
import Data.Aeson.Lens (key, _Bool)
import Data.Aeson.QQ (aesonQQ)
import Data.Default (Default (def))
import Network.Wreq (post, responseBody)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?))

test :: ApiKey -> TestTree
test _apiKey =
  testCase "ReCaptchaV2" $ do
    let captcha =
          def @ReCaptchaV2
            & apiKey .~ _apiKey
            & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v2"
            & captchaKey .~ "6LfD3PIbAAAAAJs_eEHvoOl75_83eXSqpPSRFJ_u"
        verify result = do
          response <-
            post
              "https://2captcha.com/api/v1/captcha-demo/recaptcha/verify"
              [aesonQQ|{answer: #{result}, siteKey: #{captcha ^. captchaKey}}|]
          case response ^? responseBody . key "success" . _Bool of
            Nothing -> assertFailure "Field (success) is missing in response. This test needs to be rewritten."
            Just success -> success @? "Result failed to pass verification."
    void $ assertCaptcha captcha verify
