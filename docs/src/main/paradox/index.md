@@@ index

* [Solving captchas](solving-captchas.md)
* [Custom monad](custom-monad.md)
* [Running tests](running-tests.md)

@@@

# captcha-haskell

[![Hackage](http://img.shields.io/hackage/v/captcha-core.svg)](https://hackage.haskell.org/package/captcha-core)

## Overview

captcha-haskell is an opinionated package for integrating a variety of captcha solving services.  
This package heavily uses the [lens](https://hackage.haskell.org/package/lens) package
as shown in the examples.  
An mtl-style typeclass is provided, allowing you to use your own monad transformer stack.  
In order to fit a common interface between captcha solving services, not all captcha types are supported.  
If the package is missing a captcha type you'd like to use, consider opening an [issue](https://github.com/qwbarch/captcha-haskell/issues).

@@toc { depth=2 }
