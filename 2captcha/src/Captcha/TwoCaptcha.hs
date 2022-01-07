-- |
-- Module: Captcha.TwoCaptcha
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module exports functions that follow the pvp versioning policies.
module Captcha.TwoCaptcha
  ( module Captcha.TwoCaptcha.Internal.Error,
    module Captcha.TwoCaptcha.Internal,
  )
where

import Captcha.TwoCaptcha.Internal (TwoCaptcha)
import Captcha.TwoCaptcha.Internal.Error (TwoCaptchaError (..), TwoCaptchaErrorCode (..))
import Captcha.TwoCaptcha.Internal.Types.FunCaptcha ()
import Captcha.TwoCaptcha.Internal.Types.HCaptcha ()
import Captcha.TwoCaptcha.Internal.Types.Image ()
import Captcha.TwoCaptcha.Internal.Types.ReCaptchaV2 ()
import Captcha.TwoCaptcha.Internal.Types.ReCaptchaV3 ()
import Captcha.TwoCaptcha.Internal.Types.Text ()
