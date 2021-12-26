{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Captcha.Internal.Types.FunCaptcha where

import Captcha.Internal.Types (Proxy)
import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Default (Default)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Cookie (Cookies)

-- | Parameters for solving Arkose Lab's FunCaptcha.
data FunCaptcha a = FunCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | Url where the captcha is found.
    _captchaUrl :: Text,
    -- | FunCaptcha's __data-pkey__ value.
    _captchaKey :: Text,
    -- | FunCaptcha's __surl__ service url value.
    _serviceUrl :: Maybe Text,
    -- | User agent to be used when solving the captcha.
    _userAgent :: Maybe Text,
    -- | Proxy to be used when solving the captcha.
    _proxy :: Maybe Proxy,
    -- | Cookies to be used when solving the captcha.
    _cookies :: Cookies
  }
  deriving (Generic, Default)

makeFieldsNoPrefix ''FunCaptcha
