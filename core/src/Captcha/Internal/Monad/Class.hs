{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Captcha.Internal.Monad.Class where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wreq (Response)

-- | Abstracts over a resource using the given captcha context.
class CaptchaCtx ctx r m where
  -- | Sends a request with the given context.
  request :: ctx -> m (Response ByteString)

-- | Abstracts over a captcha solving service.
class Monad m => MonadCaptcha api m where
  -- | An error specific to the captcha solving service.
  type CaptchaError api m

  -- | Submit a task to be solved by the api service.
  createTask :: CaptchaCtx ctx r m => ctx -> m (Either (CaptchaError api m) CaptchaId)

  -- | Attempt to retrieve the answer of the captcha.
  getTask :: CaptchaId -> m (Either (CaptchaError api m) Text)

  -- |
  -- Solves a captcha by submitting it with 'createTask' and then polling with 'getTask'
  -- until the answer is ready.
  --
  -- This will poll until the configured timeout duration is past.
  -- Its default value depends on the captcha service.
  solve :: CaptchaCtx ctx r m => ctx -> m (Either (CaptchaError api m) Text)

-- | Identifier for retrieving a captcha's answer.
newtype CaptchaId = CaptchaId
  { unCaptchaId :: Text
  }
  deriving (Show, Eq, Ord)
