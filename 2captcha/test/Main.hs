import qualified Captcha.TwoCaptcha as TwoCaptcha
import qualified Captcha.TwoCaptcha.Test.FunCaptcha as FunCaptcha
import qualified Captcha.TwoCaptcha.Test.HCaptcha as HCaptcha
import qualified Captcha.TwoCaptcha.Test.Image as ImageCaptcha
import qualified Captcha.TwoCaptcha.Test.ReCaptchaV2 as ReCaptchaV2
import qualified Captcha.TwoCaptcha.Test.ReCaptchaV3 as ReCaptchaV3
import qualified Captcha.TwoCaptcha.Test.Text as TextCaptcha
import Data.String.Conversions (cs)
import System.Environment (getEnv)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain . tests . cs =<< getEnv "TWOCAPTCHA_API_KEY"
  where
    tests apiKey =
      testGroup
        "2Captcha"
        [ TwoCaptcha.testTimeout,
          TextCaptcha.test apiKey,
          ImageCaptcha.test apiKey,
          ReCaptchaV2.test apiKey,
          ReCaptchaV3.test apiKey,
          HCaptcha.test apiKey,
          FunCaptcha.test apiKey
        ]
