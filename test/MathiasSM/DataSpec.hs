-- | Tests for the generic data-file helpers
module MathiasSM.DataSpec (tests) where

import Data.Either (fromRight)
import MathiasSM.Context.Data (IconVariant (..), breakOnDot, iconVariant, lookupLeaf, nameToRelPath, renderScalar)
import MathiasSM.Yaml (decodeYaml)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | The data helpers tests
tests :: TestTree
tests =
    testGroup
        "Data"
        [ testGroup
            "nameToRelPath"
            [ testCase "keeps a flat name" $ nameToRelPath "hobbies" @?= "hobbies"
            , testCase "turns directory separators into dashes" $ nameToRelPath "team-members" @?= "team/members"
            , testCase "handles several levels" $ nameToRelPath "a-b-c" @?= "a/b/c"
            ]
        , testGroup
            "lookupLeaf"
            [ testCase "finds a scalar at the top level" $
                renderScalar <$> lookupLeaf "name" example @?= Just "Drawing"
            , testCase "finds a scalar in a nested map" $
                renderScalar <$> lookupLeaf "nested.city" example @?= Just "Lima"
            , testCase "returns nothing for a missing key" $
                lookupLeaf "missing" example @?= Nothing
            , testCase "returns nothing when a path crosses a scalar" $
                lookupLeaf "name.extra" example @?= Nothing
            ]
        , testGroup
            "renderScalar"
            [ testCase "renders strings as-is" $ leaf "name: Drawing" "name" @?= Just "Drawing"
            , testCase "renders integers without a decimal" $ leaf "n: 3" "n" @?= Just "3"
            , testCase "renders booleans as true/false" $ leaf "b: true" "b" @?= Just "true"
            , testCase "renders null as empty" $ leaf "x: null" "x" @?= Just ""
            ]
        , testGroup
            "breakOnDot"
            [ testCase "keeps a flat name whole" $ breakOnDot "hobbies" @?= ("hobbies", "")
            , testCase "splits a map field at the first dot" $ breakOnDot "foo.bar" @?= ("foo", "bar")
            , testCase "keeps the remaining dotted path together" $ breakOnDot "foo.bar.baz" @?= ("foo", "bar.baz")
            ]
        , testGroup
            "iconVariant"
            [ testCase "detects an icon href field" $ iconVariant "iconHref" @?= Just ("icon", Href)
            , testCase "detects an icon raw field" $ iconVariant "iconRaw" @?= Just ("icon", Raw)
            , testCase "keeps the base name before the suffix" $ iconVariant "faviconHref" @?= Just ("favicon", Href)
            , testCase "handles the Raw suffix on a prefixed field" $ iconVariant "faviconRaw" @?= Just ("favicon", Raw)
            , testCase "ignores a plain field" $ iconVariant "name" @?= Nothing
            , testCase "ignores a field ending in Href without icon" $ iconVariant "downloadHref" @?= Nothing
            , testCase "ignores an icon field without a suffix" $ iconVariant "icon" @?= Nothing
            , testCase "is case-sensitive" $ iconVariant "IconHref" @?= Nothing
            ]
        ]
  where
    example = fromRight (error "invalid fixture") (decodeYaml "name: Drawing\nnested:\n  city: Lima")

    -- Parses a one-field map and renders that field
    leaf yaml key = renderScalar <$> lookupLeaf key (fromRight (error "invalid fixture") (decodeYaml yaml))
