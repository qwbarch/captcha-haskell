{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Captcha.Internal.Types
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.Internal.Types where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Default (Default (def))
import Data.Text (Text)
import GHC.Generics (Generic)
import Time (Millisecond, Time)
import Web.Cookie (Cookies)

-- | 'Default' instance for 'Bool' is not defined by default.
instance Default Bool where
  def = False

-- | Proxy protocol.
data ProxyProtocol = Http | Https | Socks4 | Socks5 deriving (Show)

instance Default ProxyProtocol where
  def = Http

-- | Proxy authentication.
data ProxyAuth = ProxyAuth
  { _username :: Text,
    _password :: Text
  }
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''ProxyAuth

-- | Proxy to be used when solving a captcha.
data Proxy = Proxy
  { -- | Proxy address.
    _address :: Text,
    -- | Protocol of the proxy.
    _protocol :: ProxyProtocol,
    -- | Proxy port.
    _port :: Int,
    -- | Proxy authentication, if required.
    _auth :: Maybe ProxyAuth
  }
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''Proxy

-- | Parameters for solving a captcha with text within an image.
data ImageCaptcha = ImageCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The interval to poll for the captcha's answer.
    _pollingInterval :: Maybe (Time Millisecond),
    -- | The duration to keep polling for the answer.
    _timeoutDuration :: Maybe (Time Millisecond),
    -- | The image, encoded in base-64.
    _body :: Text
  }
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''ImageCaptcha

-- | Parameters for solving a text captcha.
data TextCaptcha = TextCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The interval to poll for the captcha's answer.
    _pollingInterval :: Maybe (Time Millisecond),
    -- | The duration to keep polling for the answer.
    _timeoutDuration :: Maybe (Time Millisecond),
    -- | The text captcha to solve.
    _body :: Text
  }
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''TextCaptcha

-- | Parameters for solving Arkose Lab's FunCaptcha.
data FunCaptcha = FunCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The interval to poll for the captcha's answer.
    _pollingInterval :: Maybe (Time Millisecond),
    -- | The duration to keep polling for the answer.
    _timeoutDuration :: Maybe (Time Millisecond),
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
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''FunCaptcha

-- | Parameters for solving Google's reCAPTCHA v2.
data ReCaptchaV2 = ReCaptchaV2
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The interval to poll for the captcha's answer.
    _pollingInterval :: Maybe (Time Millisecond),
    -- | The duration to keep polling for the answer.
    _timeoutDuration :: Maybe (Time Millisecond),
    -- | Url where the captcha is found.
    _captchaUrl :: Text,
    -- | reCAPTCHA v2's __data-sitekey__ value.
    _captchaKey :: Text,
    -- | reCAPTCHA's v2's __data-s__ value.
    _dataS :: Maybe Text,
    -- | Is the reCAPTCHA an __invisible__ or __normal__ captcha?
    _invisible :: Bool,
    -- | User agent to be used when solving the captcha.
    _userAgent :: Maybe Text,
    -- | Proxy to be used when solving the captcha.
    _proxy :: Maybe Proxy,
    -- | Cookies to be used when solving the captcha.
    _cookies :: Cookies
  }
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''ReCaptchaV2

-- | Parameters for solving Google's reCAPTCHA v3.
data ReCaptchaV3 = ReCaptchaV3
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The interval to poll for the captcha's answer.
    _pollingInterval :: Maybe (Time Millisecond),
    -- | The duration to keep polling for the answer.
    _timeoutDuration :: Maybe (Time Millisecond),
    -- | Url where the captcha is found.
    _captchaUrl :: Text,
    -- | reCAPTCHA v3's __sitekey__ value.
    _captchaKey :: Text,
    -- | reCAPTCHA v3's minimum score.
    _minScore :: Double,
    -- | reCAPTCHA v3's __action__ value.
    _action :: Maybe Text,
    -- | User agent to be used when solving the captcha.
    _userAgent :: Maybe Text,
    -- | Proxy to be used when solving the captcha.
    _proxy :: Maybe Proxy,
    -- | Cookies to be used when solving the captcha.
    _cookies :: Cookies
  }
  deriving (Generic, Default, Show)

makeFieldsNoPrefix ''ReCaptchaV3

-- | Parameters for solving hCaptcha.
data HCaptcha = HCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The interval to poll for the captcha's answer.
    _pollingInterval :: Maybe (Time Millisecond),
    -- | The duration to keep polling for the answer.
    _timeoutDuration :: Maybe (Time Millisecond),
    -- | Url where the captcha is found.
    _captchaUrl :: Text,
    -- | hCaptcha's __data-sitekey__ value.
    _captchaKey :: Text,
    -- | Is the hCaptcha an __invisible__ or __normal__ captcha?
    _invisible :: Bool,
    -- |
    -- Custom data used in some implementations of hCaptcha.
    -- Note: You must provide a matching user agent if this is used.
    _rqData :: Maybe Text,
    -- | User agent to be used when solving the captcha. Required when using 'rqData'.
    _userAgent :: Maybe Text,
    -- | Proxy to be used when solving the captcha.
    _proxy :: Maybe Proxy,
    -- | Cookies to be used when solving the captcha.
    _cookies :: Cookies
  }
  deriving (Show, Generic, Default)

makeFieldsNoPrefix ''HCaptcha
