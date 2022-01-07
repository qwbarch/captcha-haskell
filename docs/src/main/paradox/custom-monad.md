# Custom monad

This section shows how to use a custom monad transformer stack with the ``captcha-haskell`` package.

To satisfy ``MonadCaptcha``, a tiny amount of boilerplate is required by providing an
instance of ``HasCaptchaEnv``.  
We will provide this instance with the help of the lens [lens](https://hackage.haskell.org/package/lens) package.

Here are the required imports and language extensions for the example below:
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}

import Captcha (CaptchaEnv, HasCaptchaEnv (captchaEnv), ImageCaptcha, MonadCaptcha (solve), mkCaptchaEnv)
import Captcha.TwoCaptcha (TwoCaptcha)
import Control.Exception (throw)
import Control.Lens.TH (makeClassy)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Default (Default (def))
import UnliftIO (MonadIO, MonadUnliftIO)
```
A minimal ``AppM`` using [ReaderT](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:3)
to pass its environment ``AppEnv`` around implicitly:
```haskell
newtype AppM a = AppM
  { unAppM :: ReaderT AppEnv IO a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader AppEnv
    )
```
Here is a minimal ``AppEnv`` to make your ``AppM`` satisfy ``MonadCaptcha``:
```haskell
data AppEnv = AppEnv
  { _appCaptchaEnv :: CaptchaEnv
  }

makeClassy ''AppEnv

instance HasCaptchaEnv AppEnv where
  captchaEnv = appCaptchaEnv
```

Now you'll be able to use solve captchas from within your ``AppM``:
```haskell
main :: IO ()
main = do
  let captcha = def @ImageCaptcha
  captchaEnv <- mkCaptchaEnv
  runAppM (solve @TwoCaptcha captcha) (AppEnv captchaEnv) >>= \case
    Right result -> print result
    Left exception -> throw exception
```

@@@ note

Notice the ``HasCaptchaEnv`` instance, this boilerplate is required.

@@@
