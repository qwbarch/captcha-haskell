-- |
-- Module: Captcha.Internal.Request
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.Internal.Request where

import Captcha.Internal.Monad (HasCaptchaEnv (session))
import Control.Lens ((^.))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack)
import Network.Wreq (Options, Response)
import qualified Network.Wreq.Session as Session
import Network.Wreq.Types (Postable)

-- | Send a POST request with the given session from 'CaptchaEnv'.
post :: (HasCaptchaEnv r, MonadReader r m, MonadIO m, Postable a) => Options -> Text -> a -> m (Response ByteString)
post options url payload =
  ask >>= \env -> liftIO $ Session.postWith options (env ^. session) (unpack url) payload

-- | Send a GET request with the given session from 'CaptchaEnv'.
get :: (HasCaptchaEnv r, MonadReader r m, MonadIO m) => Options -> Text -> m (Response ByteString)
get options url =
  ask >>= \env -> liftIO $ Session.getWith options (env ^. session) (unpack url)
