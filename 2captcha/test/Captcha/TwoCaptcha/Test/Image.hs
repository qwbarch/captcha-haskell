{-# LANGUAGE TypeApplications #-}

module Captcha.TwoCaptcha.Test.Image where

import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), ImageCaptcha)
import Captcha.TwoCaptcha (ApiKey, assertCaptcha)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Data.String.Conversions (cs)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test :: ApiKey -> TestTree
test key =
  testCase "Image Captcha" $ do
    -- Captcha image: Td4eva
    captchaBody <- cs <$> readFile "../assets/image-captcha.txt"
    let captcha =
          def @ImageCaptcha
            & apiKey .~ key
            & body .~ captchaBody
    assertCaptcha captcha
