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
    -- | reCAPTCHA's __data-sitekey__ value.
    _captchaKey :: Text,
    -- | reCAPTCHA's __data-s__ value.
    _dataS :: Maybe Text,
    -- | Is the reCAPTCHA an __invisible__ or __normal__ captcha?
    _invisible :: Bool,
    -- | Cookies to be used when solving the captcha.
    _cookies :: Cookies
  }

makeFieldsNoPrefix ''ReCaptchaV2
