-- |
-- Module: Captcha.CapMonster
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module exports functions that follow the pvp versioning policies.
module Captcha.CapMonster
  ( module Captcha.CapMonster.Internal.Error,
    module Captcha.CapMonster.Internal,
  )
where

import Captcha.CapMonster.Internal (CapMonster)
import Captcha.CapMonster.Internal.Error (CapMonsterError (..), CapMonsterErrorCode (..))
import Captcha.CapMonster.Internal.Types.FunCaptcha ()
import Captcha.CapMonster.Internal.Types.HCaptcha ()
import Captcha.CapMonster.Internal.Types.Image ()
import Captcha.CapMonster.Internal.Types.ReCaptchaV2 ()
import Captcha.CapMonster.Internal.Types.ReCaptchaV3 ()
