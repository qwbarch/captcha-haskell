{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Captcha.Internal.Types where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Default (Default (def))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'Default' instance for 'Bool' is not defined by default.
instance Default Bool where
  def = False

-- | Proxy protocol.
data ProxyProtocol = Http | Https | Sock4 | Socks5

instance Default ProxyProtocol where
  def = Http

-- | Proxy authentication.
data ProxyAuth = ProxyAuth
  { _login :: Text,
    _password :: Text
  }
  deriving (Generic, Default)

makeFieldsNoPrefix ''ProxyAuth

-- | Proxy to be used when solving a captcha.
data Proxy = Proxy
  { -- | Protocol of the proxy.
    _protocol :: ProxyProtocol,
    -- | Proxy port.
    _port :: Int,
    -- | Proxy authentication, if required.
    _auth :: Maybe ProxyAuth
  }
  deriving (Generic, Default)

makeFieldsNoPrefix ''Proxy
