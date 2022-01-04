import qualified Captcha.CapMonster as CapMonster
import qualified Captcha.CapMonster.Test.FunCaptcha as FunCaptcha
import qualified Captcha.CapMonster.Test.Image as Image
import qualified Captcha.CapMonster.Test.ReCaptchaV2 as ReCaptchaV2
import qualified Captcha.CapMonster.Test.ReCaptchaV3 as ReCaptchaV3
import Data.String.Conversions (cs)
import System.Environment (getEnv)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . tests . cs =<< getEnv "CAPMONSTER_API_KEY"
  where
    tests apiKey =
      testGroup
        "CapMonster"
        [ CapMonster.testTimeout,
          Image.test apiKey,
          ReCaptchaV2.test apiKey,
          ReCaptchaV3.test apiKey,
          FunCaptcha.test apiKey
        ]
