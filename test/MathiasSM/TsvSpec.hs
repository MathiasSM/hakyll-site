module MathiasSM.TsvSpec (tests) where

import MathiasSM.Tsv (isYes, parseTsv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Tsv"
        [ testGroup
            "parseTsv"
            [ testCase "uses the first line as the column names" $
                parseTsv (unlines ["name\tage", "Ana\t30", "Luis\t41"])
                    @?= [[("name", "Ana"), ("age", "30")], [("name", "Luis"), ("age", "41")]]
            , testCase "skips blank lines and # comments" $
                parseTsv (unlines ["name", "", "# not a row", "Ana"])
                    @?= [[("name", "Ana")]]
            , testCase "keeps empty cells" $
                parseTsv (unlines ["a\tb\tc", "1\t\t3"])
                    @?= [[("a", "1"), ("b", ""), ("c", "3")]]
            , testCase "gives no rows for empty input" $
                parseTsv "" @?= []
            ]
        , testGroup "isYes" $
            [ testCase ("reads " ++ show cell ++ " as " ++ show expected) $
                isYes "show" [("show", cell)] @?= expected
            | (cell, expected) <- [("yes", True), ("true", True), ("1", True), ("no", False), ("", False)]
            ]
                ++ [ testCase "reads a missing column as no" $
                        isYes "show" [("other", "yes")] @?= False
                   ]
        ]
