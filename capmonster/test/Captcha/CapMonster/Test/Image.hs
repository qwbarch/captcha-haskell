{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster.Test.Image where

import Captcha.CapMonster (ApiKey, assertCaptcha)
import Captcha.CapMonster.Internal.Types.Image ()
import Captcha.Internal.Types (HasApiKey (apiKey), HasBody (body), ImageCaptcha)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))
import Data.String.Conversions (cs)
import Data.Text (toLower)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

test :: ApiKey -> TestTree
test key =
  testCase "Image Captcha" $ do
    -- Captcha image: Td4eva
    captchaBody <- cs <$> readFile "../assets/image-captcha.txt"
    let captcha =
          def @ImageCaptcha
            & apiKey .~ key
            & body .~ captchaBody
    assertCaptcha captcha ((@?= "td4eva") . toLower)
