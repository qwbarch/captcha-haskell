{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.TwoCaptcha.Test.Text where

import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), TextCaptcha)
import Captcha.TwoCaptcha (ApiKey, assertCaptcha)
import Captcha.TwoCaptcha.Internal.Types.Text ()
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test key =
  testCase "Text Captcha" $
    let captcha =
          def @TextCaptcha
            & apiKey .~ key
            & body .~ "Hello world!"
     in assertCaptcha captcha
