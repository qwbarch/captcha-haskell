{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Captcha.Internal.Monad.Class
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.Internal.Monad.Class where

import Captcha.Internal.Types (HasApiKey, HasPollingInterval, HasTimeoutDuration)
import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wreq (Response)
import Time (Millisecond, Time)

-- | Abstracts over a captcha solving service.
class Monad m => MonadCaptcha api r m where
  -- | An error specific to the captcha solving service.
  type CaptchaError api r m

  -- | Submit a task to be solved by the api service.
  createTask ::
    CaptchaRequest api ctx r m =>
    -- | The captcha to be solved.
    ctx ->
    -- | Captcha id to be used with 'getTask'.
    m (Either (CaptchaError api r m) (CaptchaId ctx))

  -- | Attempt to retrieve the answer of the captcha.
  getTask ::
    CaptchaResponse api ctx =>
    -- | The captcha service's API key.
    Text ->
    -- | The captcha to check the answer of.
    CaptchaId ctx ->
    -- | The captcha's solution.
    m (Either (CaptchaError api r m) Text)

  -- |
  -- Solves a captcha by submitting it with 'createTask' and then polling with 'getTask'
  -- until the answer is ready.
  --
  -- This will poll until the configured timeout duration is past.
  -- Its default value depends on the captcha service.
  solve ::
    ( CaptchaRequest api ctx r m,
      CaptchaResponse api ctx,
      HasApiKey ctx Text,
      HasPollingInterval ctx (Maybe (Time Millisecond)),
      HasTimeoutDuration ctx (Maybe (Time Millisecond))
    ) =>
    -- | Captcha to be solved.
    ctx ->
    -- | The captcha's solution.
    m (Either (CaptchaError api r m) Text)

-- |
-- Different captcha services have different request formats.
-- This abstracts over it and sends the correct HTTP request.
class CaptchaRequest api ctx r m where
  -- | Send a request using the given captcha context.
  request ::
    -- | The captcha to be solved.
    ctx ->
    -- | The url to send the request to.
    Text ->
    m (Response ByteString)

-- |
-- Different captcha services have different response formats.
-- This abstracts over it and provides the captcha result.
class CaptchaResponse api ctx where
  -- | Parse the captcha result from the given json.
  parseResult :: Value -> Maybe Value

-- | Identifier for retrieving a captcha's answer.
newtype CaptchaId ctx = CaptchaId
  { unCaptchaId :: Integer
  }
  deriving (Show, Eq, Ord)
