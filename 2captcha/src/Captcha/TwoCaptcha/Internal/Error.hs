{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module: Captcha.TwoCaptcha.Internal.Error
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.TwoCaptcha.Internal.Error where

import Control.Exception (Exception)
import Data.Foldable (find)
import Data.Text (Text)
import Network.HTTP.Client (HttpException)

-- | All possible errors when solving a captcha using 2Captcha.
data TwoCaptchaError
  = TwoCaptchaResponseError TwoCaptchaErrorCode
  | UnknownResponseError Text Text
  | UnknownError Text
  | NetworkError HttpException
  | TimeoutError
  deriving (Show, Exception)

-- | An error code returned by the TwoCaptcha API.
data TwoCaptchaErrorCode
  = -- | The api key you provided is invalid. Please ensure it is 32 characters long.
    WrongUserKey
  | -- | The key you've provided does not exist.
    KeyDoesNotExist
  | -- | You don't have funds in your account.
    ZeroBalance
  | -- | The __pageurl__ parameter is missing in your request.
    PageUrlMissing
  | -- |
    -- You can receive this error in two cases:
    --
    -- 1. __If you solve token-based captchas (reCAPTCHA, hCaptcha, ArkoseLabs FunCaptcha, GeeTest, etc):__
    -- the queue of your captchas that are not distributed to workers is too long.
    -- Queue limit changes dynamically and depends on total amount of captchas awaiting solution and usually it’s between 50 and 100 captchas.
    --
    -- 2. __If you solve Normal Captcha:__ your maximum rate for normal captchas is lower than current rate on the server.
    -- You can change your maximum rate in <https://2captcha.com/setting your account's settings.>
    NoSlotAvailable
  | -- | Image size is less than 100 bytes.
    ZeroCaptchaFileSize
  | -- | Image size is more than 100 kB.
    TooBigCaptchaFileSize
  | -- | Image file has unsupported extension. Accepted extensions: jpg, jpeg, gif, png.
    WrongFileExtension
  | -- | Server can't recognize image file type.
    ImageTypeNotSupported
  | -- |
    -- Server can't get file data from your POST-request.
    -- That happens if your POST-request is malformed or base64 data is not a valid base64 image.
    UploadFailure
  | -- | The request is sent from the IP that is not on the list of your allowed IPs.
    IpNotAllowed
  | -- | Your IP address is banned due to many frequent attempts to access the server using wrong authorization keys.
    IpBanned
  | -- |
    -- You can get this error code when sending reCAPTCHA V2. This happens if your request contains invalid pair of googlekey and pageurl.
    -- The common reason for that is that reCAPTCHA is loaded inside an iframe hosted on another domain/subdomain.
    BadTokenOrPageUrl
  | -- | You can get this error code when sending reCAPTCHA V2. That means that sitekey value provided in your request is incorrect: it's blank or malformed.
    GoogleKeyInvalid
  | -- | The __googlekey__ parameter is missing in your request.
    GoogleKeyMissing
  | -- |
    -- You've sent an image that is marked in 2captcha's database as unrecognizable.
    -- Usually that happens if the website where you found the captcha stopped sending you captchas and started to send a "deny access" image.
    CaptchaImageBlocked
  | -- | You are sending too many unrecognizable images.
    TooManyBadImages
  | -- |
    -- You made more than 60 requests to in.php within 3 seconds.
    -- Your account is banned for 10 seconds. Ban will be lifted automatically.
    RateLimited
  | -- |
    -- The error code is returned if some required parameters are missing in your request or the values have incorrect format.
    -- For example if you submit <https://2captcha.com/2captcha-api#grid Grid images> but your request is missing an instruction for workers.
    --
    -- Blocking time: 5 minutes.
    BadParameters
  | -- | You can get this error code when sending a captcha via proxy server which is marked as BAD by the 2captcha API.
    BadProxy
  | -- | Your captcha is not solved yet.
    CaptchaNotReady
  | -- |
    -- 2captcha was unable to solve your captcha - three of their workers were unable solve it or they didn't get an answer within 90 seconds (300 seconds for reCAPTCHA V2).
    --
    -- You will not be charged for that request.
    CaptchaUnsolvable
  | -- | You've provided captcha ID in wrong format. The ID can contain numbers only.
    WrongIdFormat
  | -- | You provided an invalid captcha id.
    WrongCaptchaId
  | -- | Error is returned when 100% accuracy feature is enabled. The error means that max numbers of tries is reached but min number of matches not found.
    BadDuplicates
  | -- |
    -- Error is returned to your <https://2captcha.com/2captcha-api#complain report> request if you already complained lots of correctly solved captchas (more than 40%).
    -- Or if more than 15 minutes passed after you submitted the captcha.
    ReportNotRecorded
  | -- | Error is returned to your <https://2captcha.com/2captcha-api#complain report request> if you are trying to report the same captcha more than once.
    DuplicateReport
  | -- |
    -- You can receive this error code when sending <https://2captcha.com/2captcha-api#solving_geetest GeeTest>.
    -- This error means the __challenge__ value you provided is expired.
    TokenExpired
  | -- | Action parameter is missing or no value is provided for __action__ parameter.
    EmptyAction
  | -- |
    -- You can get this error code if we were unable to load a captcha through your proxy server.
    -- The proxy will be marked as BAD by our API and we will not accept requests with the proxy during 10 minutes.
    -- You will recieve ERROR_BAD_PROXY code from in.php API endpoint in such case.
    ProxyConnectionFailed
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Textual representation of a 'TwoCaptchaErrorCode'
errorCode :: TwoCaptchaErrorCode -> Text
errorCode = \case
  WrongUserKey -> "ERROR_WRONG_USER_KEY"
  KeyDoesNotExist -> "ERROR_KEY_DOES_NOT_EXIST"
  ZeroBalance -> "ERROR_ZERO_BALANCE"
  PageUrlMissing -> "ERROR_PAGEURL"
  NoSlotAvailable -> "ERROR_NO_SLOT_AVAILABLE"
  ZeroCaptchaFileSize -> "ERROR_ZERO_CAPTCHA_FILESIZE"
  TooBigCaptchaFileSize -> "ERROR_TOO_BIG_CAPTCHA_FILESIZE"
  WrongFileExtension -> "ERROR_WRONG_FILE_EXTENSION"
  ImageTypeNotSupported -> "ERROR_IMAGE_TYPE_NOT_SUPPORTED"
  UploadFailure -> "ERROR_UPLOAD"
  IpNotAllowed -> "ERROR_IP_NOT_ALLOWED"
  IpBanned -> "IP_BANNED"
  BadTokenOrPageUrl -> "ERROR_BAD_TOKEN_OR_PAGEURL"
  GoogleKeyInvalid -> "ERROR_GOOGLEKEY"
  GoogleKeyMissing -> "ERROR_WRONG_GOOGLEKEY"
  CaptchaImageBlocked -> "ERROR_CAPTCHAIMAGE_BLOCKED"
  TooManyBadImages -> "TOO_MANY_BAD_IMAGES"
  RateLimited -> "MAX_USER_TURN"
  BadParameters -> "ERROR_BAD_PARAMETERS"
  BadProxy -> "ERROR_BAD_PROXY"
  CaptchaNotReady -> "CAPCHA_NOT_READY"
  CaptchaUnsolvable -> "ERROR_CAPTCHA_UNSOLVABLE"
  WrongIdFormat -> "ERROR_WRONG_ID_FORMAT"
  WrongCaptchaId -> "ERROR_WRONG_CAPTCHA_ID"
  BadDuplicates -> "ERROR_BAD_DUPLICATES"
  ReportNotRecorded -> "ERROR_REPORT_NOT_RECORDED"
  DuplicateReport -> "ERROR_DUPLICATE_REPORT"
  TokenExpired -> "ERROR_TOKEN_EXPIRED"
  EmptyAction -> "ERROR_EMPTY_ACTION"
  ProxyConnectionFailed -> "ERROR_PROXY_CONNECTION_FAILED"

-- | Parse an error code into its equivalent 'TwoCaptchaErrorCode'.
parseError :: Text -> Maybe TwoCaptchaErrorCode
parseError code = find ((== code) . errorCode) [minBound .. maxBound]
