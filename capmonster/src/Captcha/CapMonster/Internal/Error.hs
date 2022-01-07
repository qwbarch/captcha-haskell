{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module: Captcha.CapMonster.Internal.Error
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.CapMonster.Internal.Error where

import Control.Exception (Exception)
import Data.Foldable (find)
import Data.Text (Text)
import Network.HTTP.Client (HttpException)

-- | All possible errors when solving a captcha using CapMonster.
data CapMonsterError
  = -- | An error returned by the CapMonster API.
    CapMonsterResponseError CapMonsterErrorCode
  | -- |
    -- An error returned by the CapMonster API that
    -- does not exist as a 'CapMonsterErrorCode' yet.
    --
    -- This error holds the error code, followed by its description.
    UnknownResponseError Text Text
  | -- |
    -- An unknown error occurred. Check the message for more details.
    --
    -- This should never occur. Please report this issue on github if this happens to you.
    UnknownError Text
  | -- | An error when sending the http request.
    NetworkError HttpException
  | -- | The captcha took to long to solve and was timed out.
    TimeoutError
  deriving (Show, Exception)

-- | An error code returned by the CapMonster API.
data CapMonsterErrorCode
  = -- | The provided API key does not exist.
    KeyDoesNotExist
  | -- | The size of the captcha must be 100 bytes or larger.
    ZeroCaptchaFileSize
  | -- | The size of the captcha must be less than 50,000 bytes.
    TooBigFileSize
  | -- | Your CapMonster balance is empty.
    ZeroBalance
  | -- | Requests from your current API key is not allowed from your ip.
    IpNotAllowed
  | -- | The captcha cannot be solved. Perhaps it's a corrupted image, or contains too much noise?
    CaptchaUnsolvable
  | -- | The captcha id does not exist. Is the captcha older than 5 minutes?
    InvalidCaptchaId
  | -- | The captcha has not been solved yet.
    CaptchaNotReady
  | -- | You have sent too many requests with an incorrect API key. Try again later.
    IpBanned
  | -- |
    -- The requested method does not exist. You should not ever have to see this error as a user.
    -- If this is ever seen, please open an issue: https://github.com/qwbarch/captcha-haskell/issues
    NoSuchMethod
  | -- | You are being rate limited. Try not to request the result of a captcha more than 1 time per 2 seconds.
    TooManyRequests
  | -- | The specified domain cannot be solved by CapMonster.
    DomainNotAllowed
  | -- | The captcha could not be solved due to no available captcha workers.
    NoSlotAvailable
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Textual representation of a 'CapMonsterErrorCode'.
errorCode :: CapMonsterErrorCode -> Text
errorCode = \case
  KeyDoesNotExist -> "ERROR_KEY_DOES_NOT_EXIST"
  ZeroCaptchaFileSize -> "ERROR_ZERO_CAPTCHA_FILESIZE"
  TooBigFileSize -> "ERROR_TOO_BIG_CAPTCHA_FILESIZE"
  ZeroBalance -> "ERROR_ZERO_BALANCE"
  IpNotAllowed -> "ERROR_IP_NOT_ALLOWED"
  CaptchaUnsolvable -> "ERROR_CAPTCHA_UNSOLVABLE"
  InvalidCaptchaId -> "ERROR_NO_SUCH_CAPCHA_ID"
  CaptchaNotReady -> "CAPTCHA_NOT_READY"
  IpBanned -> "ERROR_IP_BANNED"
  NoSuchMethod -> "ERROR_NO_SUCH_METHOD"
  TooManyRequests -> "ERROR_TOO_MUCH_REQUESTS"
  DomainNotAllowed -> "ERROR_DOMAIN_NOT_ALLOWED"
  NoSlotAvailable -> "ERROR_NO_SLOT_AVAILABLE"

-- | Parse an error code into its equivalent 'CapMonsterErrorCode'.
parseError :: Text -> Maybe CapMonsterErrorCode
parseError code = find ((== code) . errorCode) [minBound .. maxBound]
