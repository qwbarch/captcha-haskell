{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Captcha.Internal.Monad
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.Internal.Monad where

import Control.Lens.TH (makeClassy)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT))
import Network.Wreq.Session (Session, newSession)
import UnliftIO (MonadUnliftIO)

-- | Effect providing an environment required to solve captchas.
newtype Captcha a = Captcha
  { runCaptcha :: CaptchaEnv -> IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader CaptchaEnv,
      MonadUnliftIO
    )
    via ReaderT CaptchaEnv IO

-- | Provides an HTTP 'Session' to be reused for each request.
newtype CaptchaEnv = CaptchaEnv
  { _session :: Session
  }

makeClassy ''CaptchaEnv

-- | Create the environment required to solve captchas.
mkCaptchaEnv :: MonadIO m => m CaptchaEnv
mkCaptchaEnv = CaptchaEnv <$> liftIO newSession
