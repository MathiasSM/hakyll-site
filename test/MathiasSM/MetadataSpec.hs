module MathiasSM.MetadataSpec (tests) where

import Data.List (nub)
import MathiasSM.Metadata (Key (..), keyName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Metadata"
        [ testGroup
            "keyName"
            [ testCase "spells keys as they appear in front matter" $
                map keyName [Title, StartDate, ShortDescription, TOC]
                    @?= ["title", "startDate", "shortDescription", "TOC"]
            , testCase "gives every key a different spelling" $
                let spellings = map keyName [minBound .. maxBound]
                 in length (nub spellings) @?= length spellings
            ]
        ]
