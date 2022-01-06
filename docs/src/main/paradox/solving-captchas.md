# Solving captchas

By default, the ``solve`` function polls at an interval of 10 seconds, and a
timeout is not enforced.  
This can be configured for any captcha using the ``pollingInterval`` and ``timeoutDuration``
lenses.  
If you need any time-conversion functions, use the [o-clock](https://hackage.haskell.org/package/o-clock) library
(this is where the type ``Time Millisecond`` comes from).  
For simplicity, the below examples will simply rethrow any exceptions found.

@@@ note

If you don't want to poll for the answer, you can use the ``createTask`` and ``getTask`` functions directly.

@@@

## ImageCaptcha

| Name            | Type                     | Description                               |
|-----------------|--------------------------|-------------------------------------------|
| apiKey          | Text                     | API key for captcha service               |
| body            | Text                     | Base-64 encoded image                     |
| pollingInterval | Maybe (Time Millisecond) | Interval to poll for the captcha's answer |
| timeoutDuration | Maybe (Time Millisecond) | Max timeout when solving captchas         |

2Captcha
: @@snip [ImageCaptcha.hs](/src/main/paradox/examples/2captcha/minimal/ImageCaptcha.hs)

CapMonster
: @@snip [ImageCaptcha.hs](/src/main/paradox/examples/capmonster/minimal/ImageCaptcha.hs)

2Captcha (Full)
: @@snip [ImageCaptcha.hs](/src/main/paradox/examples/2captcha/full/ImageCaptcha.hs)

CapMonster (Full)
: @@snip [ImageCaptcha.hs](/src/main/paradox/examples/capmonster/full/ImageCaptcha.hs)

## TextCaptcha

| Name            | Type                     | Description                               |
|-----------------|--------------------------|-------------------------------------------|
| apiKey          | Text                     | API key for captcha service               |
| body            | Text                     | Text captcha to be solved                 |
| pollingInterval | Maybe (Time Millisecond) | Interval to poll for the captcha's answer |
| timeoutDuration | Maybe (Time Millisecond) | Max timeout when solving captchas         |

2Captcha
: @@snip [TextCaptcha.hs](/src/main/paradox/examples/2captcha/minimal/TextCaptcha.hs)

2Captcha (Full)
: @@snip [TextCaptcha.hs](/src/main/paradox/examples/2captcha/full/TextCaptcha.hs)
