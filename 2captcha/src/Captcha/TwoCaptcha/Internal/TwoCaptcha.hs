{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Captcha.TwoCaptcha.Internal.TwoCaptcha where

import Captcha.Internal.Monad (HasCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaId (CaptchaId, unCaptchaId), CaptchaRequest (request), CaptchaResponse (parseResult), MonadCaptcha (CaptchaError, createTask, getTask, solve))
import Captcha.Internal.Request (get)
import Captcha.Internal.Types (HasApiKey (apiKey), HasPassword (password), HasPollingInterval (pollingInterval), HasPort (port), HasProtocol (protocol), HasProxy (proxy), HasTimeoutDuration (timeoutDuration), HasUsername (username), Proxy (Proxy), getProxyAddress, getProxyPassword, getProxyPort, getProxyUsername)
import Captcha.TwoCaptcha.Internal.Error (TwoCaptchaError (NetworkError, TimeoutError, TwoCaptchaResponseError, UnknownError), TwoCaptchaErrorCode (CaptchaNotReady), parseError)
import Control.Arrow (ArrowChoice (left))
import Control.Error (ExceptT (ExceptT), note, runExceptT)
import Control.Lens (preview, view, (&), (.~), (^.), (^?), _Just)
import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Integer, _String, _Value)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, maybeToList)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Response, defaults, param, responseBody)
import Time (Microsecond, Millisecond, Time (Time), threadDelay, toNum)
import UnliftIO (MonadUnliftIO, timeout, try)

-- | Used for picking 'MonadCaptcha' instances for 2Captcha.
data TwoCaptcha

-- | Parse the http response into the captcha answer, handling any errors found.
parseResponse :: (Value -> Maybe Value) -> Either HttpException (Response ByteString) -> Either TwoCaptchaError Value
parseResponse f response = do
  body <- parseBody
  (status, code) <- parseStatus body
  if status == 0
    then case parseError code of
      Just exception -> Left $ TwoCaptchaResponseError exception
      Nothing -> undefined
    else note (UnknownError "TODO: WRITE ERROR") (f body)
  where
    missingResponse =
      [iii|
        The response body is missing.
        This is likely due to a change in 2Captcha's API and will need to be fixed.
      |]
    parseStatus body = note (UnknownError "TODO: WRITE ERROR MESSAGE") $ do
      status <- body ^? key "status" . _Integer
      code <- body ^? key "request" . _String
      return (status, code)
    parseBody = note (UnknownError missingResponse) =<< bimap NetworkError (preview $ responseBody . _Value) response

instance (HasCaptchaEnv r, MonadReader r m, MonadUnliftIO m) => MonadCaptcha TwoCaptcha r m where
  type CaptchaError TwoCaptcha r m = TwoCaptchaError

  createTask :: forall ctx. CaptchaRequest TwoCaptcha ctx r m => ctx -> m (Either TwoCaptchaError (CaptchaId ctx))
  createTask captcha =
    (parseCaptchaId <=< parseResponse (preview $ key "request")) <$> try (request @TwoCaptcha @ctx @r @m captcha url)
    where
      url = "https://2captcha.com/in.php"
      captchaIdMissing = [i|2Captcha did not send a captcha id after creating a task.|]
      captchaIdInvalid captchaId = [i|CaptchaId is not an Integer: #{captchaId}|]
      parseCaptchaId captchaId =
        bimap (const . UnknownError $ captchaIdInvalid captchaId) (CaptchaId . fst) . decimal
          =<< note (UnknownError captchaIdMissing) (captchaId ^? _String)

  getTask :: forall ctx. CaptchaResponse TwoCaptcha ctx => Text -> CaptchaId ctx -> m (Either TwoCaptchaError Text)
  getTask apiKey captchaId =
    fmap (view _String) . parseResponse (parseResult @TwoCaptcha @ctx) <$> try (get options url)
    where
      url = "https://2captcha.com/res.php"
      options =
        defaults
          & param "key" .~ [apiKey]
          & param "id" .~ [cs . show $ unCaptchaId captchaId]
          & param "action" .~ ["get"]
          & param "json" .~ ["1"]

  solve ::
    forall ctx.
    ( CaptchaRequest TwoCaptcha ctx r m,
      CaptchaResponse TwoCaptcha ctx,
      HasApiKey ctx Text,
      HasPollingInterval ctx (Maybe (Time Millisecond)),
      HasTimeoutDuration ctx (Maybe (Time Millisecond))
    ) =>
    ctx ->
    m (Either TwoCaptchaError Text)
  solve captcha =
    handleTimeout (captcha ^. timeoutDuration) . runExceptT $
      ExceptT . pollResult =<< ExceptT (createTask @TwoCaptcha @r @m captcha)
    where
      handleTimeout (Just duration) f = fromMaybe (Left TimeoutError) <$> timeout (toNum @Microsecond duration) f
      handleTimeout Nothing f = f
      pollResult captchaId =
        threadDelay (fromMaybe (Time @Millisecond 10_000) (captcha ^. pollingInterval))
          *> getTask @TwoCaptcha @r @m @ctx (captcha ^. apiKey) captchaId
          >>= \case
            Left (TwoCaptchaResponseError CaptchaNotReady) -> pollResult captchaId
            x -> pure x

instance CaptchaResponse TwoCaptcha ctx where
  parseResult = preview $ key "request"

parseProxyType :: HasProxy a (Maybe Proxy) => a -> [Text]
parseProxyType captcha = maybeToList $ cs . show <$> captcha ^? proxy . _Just . protocol

parseProxy :: HasProxy a (Maybe Proxy) => a -> [Text]
parseProxy captcha = maybeToList $ do
  let auth = do
        username <- getProxyUsername captcha
        password <- getProxyPassword captcha
        pure [i|#{username}:#{password}@|]
  address <- getProxyAddress captcha
  port <- getProxyPort captcha
  pure $ fromMaybe mempty auth <> [i|#{address}:#{port}|]
