{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Captcha.Internal.Types.Text where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Default (Default)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Parameters for solving a text captcha.
data TextCaptcha a = TextCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The text captcha to solve.
    _body :: Text
  }
  deriving (Generic, Default)

makeFieldsNoPrefix ''TextCaptcha
