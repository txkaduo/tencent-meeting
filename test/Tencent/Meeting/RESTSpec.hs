module Tencent.Meeting.RESTSpec where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens
import           Test.Hspec

import Tencent.Meeting
-- }}}1


spec :: Spec
spec = do
  public_params0 <- runIO $ newRestPublicParams dummy_app_id dummy_secret_id
  let public_params = public_params0
                        & rpNonce .~ 2095528619
                        & rpTimestamp .~ Timestamp 1586675334

  describe "restStringToSign" $ do
    it "works" $ do
      let expected = "POST\nX-TC-Key=aSecretId&X-TC-Nonce=2095528619&X-TC-Timestamp=1586675334\n/v1/meetings\nbody"
      restStringToSign public_params HttpPost "/v1/meetings" mempty (Just "body") `shouldBe` expected

  describe "restSignature" $ do
    it "works" $ do
      restSignature dummy_secret_key public_params HttpPost "/v1/meetings" mempty (Just "body")
        `shouldBe` "OGM4YTExZjc5MzVlNTg4ZDA0NDVhYjA0ZGNhYzYzOTZjMTM2YjU3ZmJiYzVhODBjYjY1NGYxZmMyMTVlMWRhYQ=="
  where
    dummy_app_id = AppId "1234567890"
    dummy_secret_id = SecretId "aSecretId"
    dummy_secret_key = SecretKey "aSecretKey"
