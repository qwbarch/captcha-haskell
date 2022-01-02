{-# LANGUAGE OverloadedStrings #-}

module Captcha.CapMonster.Internal.CapMonster where

import Data.Text (Text)

-- | Used for picking 'MonadCaptcha' instances for CapMonster.
data CapMonster = CapMonster

-- | CapMonster's "create task" HTTP endpoint.
createTaskUrl :: Text
createTaskUrl = "https://api.capmonster.cloud/createTask"
