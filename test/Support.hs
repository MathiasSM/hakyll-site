-- | Small helpers that keep the specs short and readable
module Support (
  group,
  it,
  Fields,
  render,
  set,
  without,
  day,
) where

import Data.Time (Day, fromGregorian)
import Test.HUnit (Test (TestLabel, TestList))

-- | A named set of tests
group :: String -> [Test] -> Test
group name = TestLabel name . TestList

-- | One named expectation, e.g. @it "uses the first line as column names" $ actual ~?= expected@
it :: String -> Test -> Test
it = TestLabel

{- | Key/value pairs for building YAML (project files, post front matter).

Start from a valid fixture and change one thing per test with 'set' and 'without'.
-}
type Fields = [(String, String)]

-- | The fields as YAML text, one @key: value@ per line
render :: Fields -> String
render fields = unlines [key ++ ": " ++ value | (key, value) <- fields]

-- | Replaces a field's value, or adds the field
set :: String -> String -> Fields -> Fields
set key value fields
  | key `elem` map fst fields = [(k, if k == key then value else v) | (k, v) <- fields]
  | otherwise = fields ++ [(key, value)]

-- | Drops fields by name
without :: [String] -> Fields -> Fields
without keys = filter ((`notElem` keys) . fst)

day :: Integer -> Int -> Int -> Day
day = fromGregorian
