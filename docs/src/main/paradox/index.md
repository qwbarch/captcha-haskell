# captcha-haskell

## Overview

captcha-haskell is an opinionated package for integrating a variety of captcha solving service.  
This package heavily uses the [lens](https://hackage.haskell.org/package/lens) package
as shown in the below example.  
An mtl-style typeclass is provided, allowing you to use your own monad transformer stack.  
In order to fit a common interface between captcha solving services, not all captcha types are supported.  
If the package is missing a captcha type you'd like to use, consider opening an [issue](https://github.com/qwbarch/captcha-haskell/issues).

## Quick example

Here is a minimal example to solve reCAPTCHA v2 using [2Captcha](https://2captcha.com).

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Captcha (Captcha (runCaptcha), HasApiKey (apiKey), HasCaptchaKey (captchaKey), HasCaptchaUrl (captchaUrl), MonadCaptcha (solve), ReCaptchaV2, mkCaptchaEnv)
import Captcha.TwoCaptcha (TwoCaptcha)
import Control.Exception (throw)
import Control.Lens ((&), (.~))
import Data.Default (Default (def))

main :: IO ()
main = do
  let captcha =
        def @ReCaptchaV2
          & apiKey .~ "YOUR_API_KEY"
          & captchaUrl .~ "https://2captcha.com/demo/recaptcha-v2"
          & captchaKey .~ "6LeIxboZAAAAAFQy7d8GPzgRZu2bV0GwKS8ue_cH"
  captchaEnv <- mkCaptchaEnv
  runCaptcha (solve @TwoCaptcha captcha) captchaEnv >>= \case
    Right result -> print result
    Left exception -> throw exception
```
