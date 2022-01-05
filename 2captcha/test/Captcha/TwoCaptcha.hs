{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.TwoCaptcha where

import Captcha.Internal.Monad (Captcha (runCaptcha), CaptchaEnv, mkCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest, CaptchaResponse, MonadCaptcha (solve))
import Captcha.Internal.Types
import Captcha.TwoCaptcha.Internal.Error (TwoCaptchaError (TimeoutError, TwoCaptchaResponseError), TwoCaptchaErrorCode (NoSlotAvailable))
import Captcha.TwoCaptcha.Internal.TwoCaptcha (TwoCaptcha)
import Captcha.TwoCaptcha.Internal.Types.Image ()
import Control.Lens ((&), (?~))
import Data.Default (Default (def))
import Data.Text (Text)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Time (Millisecond, Time (Time))

type ApiKey = Text

assertCaptcha ::
  ( CaptchaRequest TwoCaptcha ctx CaptchaEnv Captcha,
    CaptchaResponse TwoCaptcha ctx,
    HasApiKey ctx Text,
    HasPollingInterval ctx (Maybe (Time Millisecond)),
    HasTimeoutDuration ctx (Maybe (Time Millisecond))
  ) =>
  ctx ->
  IO ()
assertCaptcha captcha =
  mkCaptchaEnv >>= runCaptcha (solve @TwoCaptcha captcha) >>= \case
    Left (TwoCaptchaResponseError NoSlotAvailable) -> pure ()
    Right x -> print x
    Left exception -> assertFailure $ show exception

testTimeout :: TestTree
testTimeout =
  testCase "Captcha Timeout" $ do
    let captcha = def @ImageCaptcha & timeoutDuration ?~ Time @Millisecond 0
    mkCaptchaEnv >>= runCaptcha (solve @TwoCaptcha captcha) >>= \case
      Left TimeoutError -> pure ()
      Left exception -> assertFailure $ show exception
      Right _ -> assertFailure "Captcha did not timeout."
