{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Captcha.CapMonster where

import Captcha.CapMonster.Internal (CapMonster)
import Captcha.CapMonster.Internal.Error (CapMonsterError (CapMonsterResponseError, TimeoutError), CapMonsterErrorCode (NoSlotAvailable))
import Captcha.CapMonster.Internal.Types.Image ()
import Captcha.Internal.Monad (Captcha (runCaptcha), CaptchaEnv, mkCaptchaEnv)
import Captcha.Internal.Monad.Class (CaptchaRequest, CaptchaResponse, MonadCaptcha (solve))
import Captcha.Internal.Types (HasApiKey, HasPollingInterval, HasTimeoutDuration (timeoutDuration), ImageCaptcha)
import Control.Lens ((?~))
import Data.Default (Default (def))
import Data.Function ((&))
import Data.Text (Text)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Time (Millisecond, Time (Time))

type ApiKey = Text

assertCaptcha ::
  ( CaptchaRequest CapMonster ctx CaptchaEnv Captcha,
    CaptchaResponse CapMonster ctx,
    HasApiKey ctx Text,
    HasPollingInterval ctx (Maybe (Time Millisecond)),
    HasTimeoutDuration ctx (Maybe (Time Millisecond))
  ) =>
  ctx ->
  IO ()
assertCaptcha captcha =
  mkCaptchaEnv >>= runCaptcha (solve @CapMonster captcha) >>= \case
    Left (CapMonsterResponseError NoSlotAvailable) -> pure ()
    Right _ -> pure ()
    Left exception -> assertFailure $ show exception

testTimeout :: TestTree
testTimeout =
  testCase "Captcha Timeout" $ do
    let captcha = def @ImageCaptcha & timeoutDuration ?~ Time @Millisecond 0
    mkCaptchaEnv >>= runCaptcha (solve @CapMonster captcha) >>= \case
      Left TimeoutError -> pure ()
      Left exception -> assertFailure $ show exception
      Right _ -> assertFailure "Captcha did not timeout."
