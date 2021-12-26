{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Captcha.Internal.Types.ReCaptcha where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Text (Text)
import Web.Cookie (Cookies)

-- | Parameters for solving Google's reCAPTCHA v2.
data ReCaptchaV2 = ReCaptchaV2
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | Url where the captcha is found.
    _captchaUrl :: Text,
    -- | reCAPTCHA v2's __data-sitekey__ value.
    _captchaKey :: Text,
    -- | reCAPTCHA's v2's __data-s__ value.
    _dataS :: Maybe Text,
    -- | Is the reCAPTCHA an __invisible__ or __normal__ captcha?
    _invisible :: Bool,
    -- | Cookies to be used when solving the captcha.
    _cookies :: Cookies
  }

makeFieldsNoPrefix ''ReCaptchaV2

-- | Parameters for solving Google's reCAPTCHA v3.
data ReCaptchaV3 = ReCaptchaV3
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | Url where the captcha is found.
    _captchaUrl :: Text,
    -- | reCAPTCHA v3's __sitekey__ value.
    _captchaKey :: Text,
    -- | reCAPTCHA v3's minimum score.
    _minScore :: Double,
    -- | reCAPTCHA v3's __action__ value.
    _action :: Maybe Text
  }

makeFieldsNoPrefix ''ReCaptchaV3
