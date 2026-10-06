import Control.Monad (unless)
import Data.List (nub)
import MathiasSM.CleanURL (cleanIndex)
import MathiasSM.Metadata (Key, keyName)
import MathiasSM.Tsv (isYes, parseTsv)
import System.Exit (exitFailure)
import Test.HUnit

tests :: Test
tests =
  TestList
    [ "parseTsv: header names the columns" ~:
        parseTsv "a\tb\n1\t2\n3\t4" ~?= [[("a", "1"), ("b", "2")], [("a", "3"), ("b", "4")]]
    , "parseTsv: skips blank and comment lines" ~:
        parseTsv "a\n\n# note\n1\n" ~?= [[("a", "1")]]
    , "parseTsv: keeps empty cells" ~:
        parseTsv "a\tb\tc\n1\t\t3" ~?= [[("a", "1"), ("b", ""), ("c", "3")]]
    , "parseTsv: empty input" ~: parseTsv "" ~?= []
    , "isYes: accepts yes/true/1" ~:
        map (\v -> isYes "x" [("x", v)]) ["yes", "true", "1", "no", ""] ~?= [True, True, True, False, False]
    , "isYes: missing column is false" ~: isYes "x" [("y", "yes")] ~?= False
    , "cleanIndex: root index" ~: cleanIndex "/index.html" ~?= "/"
    , "cleanIndex: nested index" ~: cleanIndex "/blog/index.html" ~?= "/blog"
    , "cleanIndex: leaves other URLs" ~:
        map cleanIndex ["/blog/post.html", "https://x.org/a"] ~?= ["/blog/post.html", "https://x.org/a"]
    , "keyName: every key has a distinct spelling" ~:
        let names = map keyName [minBound .. maxBound :: Key]
         in length (nub names) ~?= length names
    ]

main :: IO ()
main = do
  Counts{errors, failures} <- runTestTT tests
  unless (errors + failures == 0) exitFailure
