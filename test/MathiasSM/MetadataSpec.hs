module MathiasSM.MetadataSpec (tests) where

import Data.List (nub)
import MathiasSM.Metadata (Key (..), keyName)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "Metadata"
    [ group
        "keyName"
        [ it "spells keys as they appear in front matter" $
            map keyName [Title, StartDate, ShortDescription, TOC]
              ~?= ["title", "startDate", "shortDescription", "TOC"]
        , it "gives every key a different spelling" $
            let spellings = map keyName [minBound .. maxBound]
             in length (nub spellings) ~?= length spellings
        ]
    ]
