{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Captcha.Internal.Monad where

import Control.Lens.TH (makeClassy)
import Control.Monad.Reader (ReaderT (runReaderT))
import Network.Wreq.Session (Session, newAPISession)
import UnliftIO (MonadIO, MonadUnliftIO)

-- | @newtype@ over 'ReaderT' containing the HTTP 'Session'.
newtype Captcha a = Captcha
  { unCaptcha :: ReaderT CaptchaEnv IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO
    )

-- | HTTP 'Session' to be reused for each request.
newtype CaptchaEnv = CaptchaEnv
  { _session :: Session
  }

makeClassy ''CaptchaEnv

-- | Run the given 'Captcha', using an implicit HTTP 'Session'.
runCaptcha :: Captcha a -> IO a
runCaptcha captcha = newAPISession >>= \session -> runReaderT (unCaptcha captcha) (CaptchaEnv session)
