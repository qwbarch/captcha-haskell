{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Captcha.CapMonster.Internal.CapMonster where

import Captcha.CapMonster.Internal.Error (CapMonsterError (CapMonsterResponseError, NetworkError, TimeoutError, UnknownError, UnknownResponseError), CapMonsterErrorCode (CaptchaNotReady))
import qualified Captcha.CapMonster.Internal.Error as CapMonsterError
import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaId (CaptchaId, unCaptchaId), CaptchaRequest (request), CaptchaResponse (parseResult), MonadCaptcha (CaptchaError, createTask, getTask, solve))
import Captcha.Internal.Request (post)
import Captcha.Internal.Types (HasApiKey (apiKey), HasPollingInterval (pollingInterval), HasTimeoutDuration (timeoutDuration))
import Control.Error (note)
import Control.Lens (preview, view, (^.), (^?))
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Integer, _String, _Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy (ByteString)
import Data.Either.Extra (fromEither)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Response, defaults, responseBody)
import Time (Microsecond, Millisecond, Time (Time), toNum)
import Time.Units (threadDelay)
import UnliftIO (MonadUnliftIO, timeout, try)

-- | Used for picking 'MonadCaptcha' instances for CapMonster.
data CapMonster

-- | CapMonster's "create task" HTTP endpoint.
createTaskUrl :: Text
createTaskUrl = "https://api.capmonster.cloud/createTask"

-- | Parse the http response into the captcha answer, handling any errors found.
parseResponse :: (Value -> Maybe Value) -> Either HttpException (Response ByteString) -> Either CapMonsterError Value
parseResponse f response =
  parseBody >>= \body -> maybe (Left . parseError $ Just body) Right (f body)
  where
    missingResponse =
      [iii|
          The response body is missing.
          This is likely due to a change in CapMonster's API and will need to be fixed.
        |]
    readError responseBody = do
      body <- responseBody
      code <- body ^? key "errorCode" . _String
      description <- body ^? key "errorDescription" . _String
      return (code, description)
    parseError responseBody = fromEither $ do
      (code, description) <- note (CapMonsterResponseError CaptchaNotReady) (readError responseBody)
      note (UnknownResponseError code description) (CapMonsterResponseError <$> CapMonsterError.parseError code)
    parseBody = note (UnknownError missingResponse) =<< bimap NetworkError (preview $ responseBody . _Value) response

instance (HasCaptchaEnv r, MonadReader r m, MonadUnliftIO m) => MonadCaptcha CapMonster r m where
  type CaptchaError CapMonster r m = CapMonsterError

  createTask :: forall ctx. CaptchaRequest CapMonster ctx r m => ctx -> m (Either CapMonsterError (CaptchaId ctx))
  createTask captcha =
    (parseCaptchaId <=< parseResponse (preview $ key "taskId")) <$> try (request @CapMonster @ctx @r @m captcha)
    where
      parseCaptchaId captchaId =
        CaptchaId <$> note (UnknownError [i|CaptchaId is not an Integer: #{captchaId}|]) (captchaId ^? _Integer)

  getTask :: forall ctx. CaptchaResponse CapMonster ctx => Text -> CaptchaId ctx -> m (Either CapMonsterError Text)
  getTask apiKey captchaId =
    fmap (view _String) . parseResponse (parseResult @CapMonster @ctx) <$> try (post defaults url payload)
    where
      url = "https://api.capmonster.cloud/getTaskResult"
      payload =
        [aesonQQ|
          {
            clientKey: #{apiKey},
            taskId: #{unCaptchaId captchaId}
          }
        |]

  solve ::
    forall ctx.
    ( CaptchaRequest CapMonster ctx r m,
      CaptchaResponse CapMonster ctx,
      HasApiKey ctx Text,
      HasPollingInterval ctx (Maybe (Time Millisecond)),
      HasTimeoutDuration ctx (Maybe (Time Millisecond))
    ) =>
    ctx ->
    m (Either CapMonsterError Text)
  solve captcha =
    handleTimeout (captcha ^. timeoutDuration) . runExceptT $
      ExceptT . pollResult =<< ExceptT (createTask @CapMonster @r @m captcha)
    where
      handleTimeout (Just duration) f = fromMaybe (Left TimeoutError) <$> timeout (toNum @Microsecond $ duration) f
      handleTimeout Nothing f = f
      pollResult captchaId =
        threadDelay (fromMaybe (Time @Millisecond 10_000) (captcha ^. pollingInterval))
          *> getTask @CapMonster @r @m @ctx (captcha ^. apiKey) captchaId
          >>= \case
            Left (CapMonsterResponseError CaptchaNotReady) -> pollResult captchaId
            x -> pure x
