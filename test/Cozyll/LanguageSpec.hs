-- | Tests for the language code mapping
module Cozyll.LanguageSpec (tests) where

import Cozyll.Domain.Language (Language (..), languageCode)
import Data.List (nub)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Language"
        [ testCase "has a code for each language" $
            map languageCode [En, Es, Ja] @?= ["en", "es", "ja"]
        , testCase "has distinct codes" $
            let codes = map languageCode [minBound .. maxBound]
             in length (nub codes) @?= length codes
        ]
