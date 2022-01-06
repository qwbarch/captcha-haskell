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
: @@snip [ImageCaptcha.hs](/src/main/resources/examples/2captcha/minimal/ImageCaptcha.hs)

CapMonster
: @@snip [ImageCaptcha.hs](/src/main/resources/examples/capmonster/minimal/ImageCaptcha.hs)

2Captcha (Full)
: @@snip [ImageCaptcha.hs](/src/main/resources/examples/2captcha/full/ImageCaptcha.hs)

CapMonster (Full)
: @@snip [ImageCaptcha.hs](/src/main/resources/examples/capmonster/full/ImageCaptcha.hs)

## TextCaptcha

| Name            | Type                     | Description                               |
|-----------------|--------------------------|-------------------------------------------|
| apiKey          | Text                     | API key for captcha service               |
| body            | Text                     | Text captcha to be solved                 |
| pollingInterval | Maybe (Time Millisecond) | Interval to poll for the captcha's answer |
| timeoutDuration | Maybe (Time Millisecond) | Max timeout when solving captchas         |

2Captcha
: @@snip [TextCaptcha.hs](/src/main/resources/examples/2captcha/minimal/TextCaptcha.hs)

2Captcha (Full)
: @@snip [TextCaptcha.hs](/src/main/resources/examples/2captcha/full/TextCaptcha.hs)

## FunCaptcha

| Name            | Type                     | Description                                    |
|-----------------|--------------------------|------------------------------------------------|
| apiKey          | Text                     | API key for captcha service                    |
| captchaUrl      | Text                     | Url where the captcha is found                 |
| captchaKey      | Text                     | FunCaptcha's __data-pkey__ value               |
| serviceUrl      | Maybe Text               | FunCaptcha's __surl__ value                    |
| userAgent       | Maybe Text               | User agent to be used when solving the captcha |
| proxy           | Maybe Proxy              | Proxy to be used when solving the captcha      |
| cookies         | Cookies                  | Cookies to be used when solving the captcha    |
| pollingInterval | Maybe (Time Millisecond) | Interval to poll for the captcha's answer      |
| timeoutDuration | Maybe (Time Millisecond) | Max timeout when solving captchas              |

2Captcha
: @@snip [FunCaptcha.hs](/src/main/resources/examples/2captcha/minimal/FunCaptcha.hs)

CapMonster
: @@snip [FunCaptcha.hs](/src/main/resources/examples/capmonster/minimal/FunCaptcha.hs)

2Captcha (Full)
: @@snip [FunCaptcha.hs](/src/main/resources/examples/2captcha/full/FunCaptcha.hs)

CapMonster (Full)
: @@snip [FunCaptcha.hs](/src/main/resources/examples/capmonster/full/FunCaptcha.hs)

## ReCaptchaV2

| Name            | Type                     | Description                                    |
|-----------------|--------------------------|------------------------------------------------|
| apiKey          | Text                     | API key for captcha service                    |
| invisible       | Boolean                  | Is the captcha using the invisible variant?    |
| captchaUrl      | Text                     | Url where the captcha is found                 |
| captchaKey      | Text                     | reCAPTCHA v2's __data-sitekey__ value          |
| dataS           | Maybe Text               | reCAPTCHA v2's __data-s__ value              |
| userAgent       | Maybe Text               | User agent to be used when solving the captcha |
| proxy           | Maybe Proxy              | Proxy to be used when solving the captcha      |
| cookies         | Cookies                  | Cookies to be used when solving the captcha    |
| pollingInterval | Maybe (Time Millisecond) | Interval to poll for the captcha's answer      |
| timeoutDuration | Maybe (Time Millisecond) | Max timeout when solving captchas              |

2Captcha
: @@snip [ReCaptchaV2.hs](/src/main/resources/examples/2captcha/minimal/ReCaptchaV2.hs)

CapMonster
: @@snip [ReCaptchaV2.hs](/src/main/resources/examples/capmonster/minimal/ReCaptchaV2.hs)

2Captcha (Full)
: @@snip [ReCaptchaV2.hs](/src/main/resources/examples/2captcha/full/ReCaptchaV2.hs)

CapMonster (Full)
: @@snip [ReCaptchaV2.hs](/src/main/resources/examples/capmonster/full/ReCaptchaV2.hs)

## ReCaptchaV3

| Name            | Type                     | Description                                    |
|-----------------|--------------------------|------------------------------------------------|
| apiKey          | Text                     | API key for captcha service                    |
| minScore        | Double                   | reCAPTCHAV3's minimum score                    |
| captchaUrl      | Text                     | Url where the captcha is found                 |
| captchaKey      | Text                     | reCAPTCHA v3's __sitekey__ value               |
| action          | Maybe Text               | reCAPTCHA v3's __action__ value                |
| userAgent       | Maybe Text               | User agent to be used when solving the captcha |
| proxy           | Maybe Proxy              | Proxy to be used when solving the captcha      |
| cookies         | Cookies                  | Cookies to be used when solving the captcha    |
| pollingInterval | Maybe (Time Millisecond) | Interval to poll for the captcha's answer      |
| timeoutDuration | Maybe (Time Millisecond) | Max timeout when solving captchas              |

2Captcha
: @@snip [ReCaptchaV3.hs](/src/main/resources/examples/2captcha/minimal/ReCaptchaV3.hs)

CapMonster
: @@snip [ReCaptchaV3.hs](/src/main/resources/examples/capmonster/minimal/ReCaptchaV3.hs)

2Captcha (Full)
: @@snip [ReCaptchaV3.hs](/src/main/resources/examples/2captcha/full/ReCaptchaV3.hs)

CapMonster (Full)
: @@snip [ReCaptchaV3.hs](/src/main/resources/examples/capmonster/full/ReCaptchaV3.hs)
