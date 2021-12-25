{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Captcha.Internal.Types.Image where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Default (Default)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Parameters for solving a captcha with text within an image.
data ImageCaptcha a = ImageCaptcha
  { -- | The captcha solver's API key.
    _apiKey :: Text,
    -- | The image, encoded in base-64.
    _body :: Text
  }
  deriving (Generic, Default)

makeFieldsNoPrefix ''ImageCaptcha
