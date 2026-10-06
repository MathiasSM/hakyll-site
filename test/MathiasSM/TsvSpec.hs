module MathiasSM.TsvSpec (tests) where

import MathiasSM.Tsv (isYes, parseTsv)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "Tsv"
    [ group
        "parseTsv"
        [ it "uses the first line as the column names" $
            parseTsv (unlines ["name\tage", "Ana\t30", "Luis\t41"])
              ~?= [[("name", "Ana"), ("age", "30")], [("name", "Luis"), ("age", "41")]]
        , it "skips blank lines and # comments" $
            parseTsv (unlines ["name", "", "# not a row", "Ana"])
              ~?= [[("name", "Ana")]]
        , it "keeps empty cells" $
            parseTsv (unlines ["a\tb\tc", "1\t\t3"])
              ~?= [[("a", "1"), ("b", ""), ("c", "3")]]
        , it "gives no rows for empty input" $
            parseTsv "" ~?= []
        ]
    , group "isYes" $
        [ it ("reads " ++ show cell ++ " as " ++ show expected) $
            isYes "show" [("show", cell)] ~?= expected
        | (cell, expected) <- [("yes", True), ("true", True), ("1", True), ("no", False), ("", False)]
        ]
          ++ [ it "reads a missing column as no" $
                 isYes "show" [("other", "yes")] ~?= False
             ]
    ]
