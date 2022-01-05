import qualified Captcha.TwoCaptcha as TwoCaptcha
import qualified Captcha.TwoCaptcha.Test.Image as Image
import qualified Captcha.TwoCaptcha.Test.ReCaptchaV2 as ReCaptchaV2
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
          Image.test apiKey,
          ReCaptchaV2.test apiKey
        ]
