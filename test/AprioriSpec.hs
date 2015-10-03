-- |
--
-- Module      : AprioriSpec
-- Description : Tests for "".
-- License     :  MIT
-- Stability   : development.

module AprioriSpec (
  spec
) where

import Test.Hspec

import AprioriSpec.Data
import AprioriSpec.Data.TiloBalkeExample

spec :: Spec
spec = do
    describe "Prelude.head" $ do
      it  "aaa" $ do
        1 `shouldBe` 1




