{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Captcha.Internal.Monad.Class where

import Data.Text (Text)

-- |
-- 'MonadCaptcha' provides an interface over 'solve', using
-- the given API context.
--
-- For examples, please refer to the README.
class Monad m => MonadCaptcha ctx m where
  -- | An error returned by the captcha service.
  type CaptchaError ctx

  -- | Submit the given captcha to be solved by the captcha service.
  submit :: ctx -> m (Either (CaptchaError ctx) CaptchaId)

  -- | Attempt to retrieve the result of the solved captcha.
  result :: CaptchaId -> m (Either (CaptchaError ctx) CaptchaResult)

  -- |
  -- Combines 'submit' and 'result', where the captcha is
  -- submitted to the captcha service, and then the answer is polled
  -- until the answer is ready.
  --
  -- This function polls at an interval set by the given context.
  -- A timeout duration is also set by the given context.
  solve :: ctx -> m (Either (CaptchaError ctx) CaptchaResult)

-- | An identifier given from the captcha solving service.
type CaptchaId = Text

-- | The result of the solved captcha.
type CaptchaResult = Text
