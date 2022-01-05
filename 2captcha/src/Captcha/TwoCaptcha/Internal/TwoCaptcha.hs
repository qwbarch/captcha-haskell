{-# LANGUAGE OverloadedStrings #-}

module Captcha.TwoCaptcha.Internal.TwoCaptcha where

import Data.Text (Text)

-- | Used for picking 'MonadCaptcha' instances for 2Captcha.
data TwoCaptcha
