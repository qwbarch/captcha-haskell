{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Captcha.Internal
-- Copyright: (c) 2022 Edward Yang
-- License: MIT
--
-- This module is for internal-use and does not follow pvp versioning policies.
module Captcha.Internal where

import Captcha.Internal.Types (HasAddress (address), HasAuth (auth), HasCookies (cookies), HasPassword (password), HasPort (port), HasProtocol (protocol), HasProxy (proxy), HasUsername (username), Proxy)
import Control.Lens (preview, view, (^?), _Just)
import Data.ByteString.Builder (toLazyByteString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Web.Cookie (Cookies)
import qualified Web.Cookie as Cookie

-- | Render the cookies as a lazy text.
renderCookies :: HasCookies a Cookies => a -> Text
renderCookies = toStrict . decodeUtf8 . toLazyByteString . Cookie.renderCookies . view cookies

-- | Retrieve the proxy's type as, converted into 'Text'.
getProxyType :: HasProxy a (Maybe Proxy) => a -> Maybe Text
getProxyType captcha = cs . show <$> captcha ^? proxy . _Just . protocol

-- | Retrieve the proxy's host address.
getProxyAddress :: HasProxy a (Maybe Proxy) => a -> Maybe Text
getProxyAddress = preview $ proxy . _Just . address

-- | Retrieve the proxy's port.
getProxyPort :: HasProxy a (Maybe Proxy) => a -> Maybe Int
getProxyPort = preview $ proxy . _Just . port

-- | Retrieve the proxy's authentication username.
getProxyUsername :: HasProxy a (Maybe Proxy) => a -> Maybe Text
getProxyUsername = preview $ proxy . _Just . auth . _Just . username

-- | Retrieve the proxy's authentication password.
getProxyPassword :: HasProxy a (Maybe Proxy) => a -> Maybe Text
getProxyPassword = preview $ proxy . _Just . auth . _Just . password
