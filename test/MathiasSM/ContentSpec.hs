-- | Tests for the front-matter parsers (posts, aliases, menus)
module MathiasSM.ContentSpec (tests) where

import Data.Aeson (Value (Bool))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.List (nub)
import MathiasSM.Config (Env (..))
import MathiasSM.Content
import MathiasSM.TestUtils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | The front-matter parsing tests
tests :: TestTree
tests =
    testGroup
        "Content"
        [ languageTests
        , postTests
        , isHiddenTests
        , aliasTests
        , navTests
        ]

-- Language -------------------------------------------------------------------

languageTests :: TestTree
languageTests =
    testGroup
        "Language"
        [ testCase "has a code for each language" $
            map languageCode [En, Es, Ja] @?= ["en", "es", "ja"]
        , testCase "has distinct codes" $
            let codes = map languageCode [minBound .. maxBound]
             in length (nub codes) @?= length codes
        ]

-- Posts ----------------------------------------------------------------------

-- | The least a post needs
validPost :: Fields
validPost = [("title", "A post"), ("date", "2026-10-06"), ("path", "/blog/a-post")]

postTests :: TestTree
postTests =
    testGroup
        "parsePostYaml"
        [ testCase "accepts a post with only title, date and path" $
            parse validPost
                @?= Right (Post "A post" (day 2026 10 6) "/blog/a-post" En Nothing False)
        , testCase "reads language, last modification and draft" $
            parse (set "language" "es" $ set "lastModifiedAt" "2027-01-02" $ set "draft" "true" validPost)
                @?= Right (Post "A post" (day 2026 10 6) "/blog/a-post" Es (Just (day 2027 1 2)) True)
        , testCase "rejects a lastModifiedAt before the date" $
            parse (set "lastModifiedAt" "2026-09-01" validPost)
                @?= Left ["lastModifiedAt: is before date"]
        , testCase "allows a lastModifiedAt on the date" $
            parse (set "lastModifiedAt" "2026-10-06" validPost)
                @?= Right (Post "A post" (day 2026 10 6) "/blog/a-post" En (Just (day 2026 10 6)) False)
        , testCase "ignores keys it doesn't model, even empty ones" $
            parse (set "description" "" $ set "TOC" "true" validPost)
                @?= parse validPost
        , testGroup "requires" $
            [ testCase key $ parse (without [key] validPost) @?= Left [key ++ ": missing"]
            | key <- ["title", "date", "path"]
            ]
        , testCase "rejects a date that doesn't exist" $
            parse (set "date" "2026-13-45" validPost)
                @?= Left ["date: could not parse date: invalid day:(2026,13,45)"]
        , testCase "rejects an unknown language" $
            parse (set "language" "fr" validPost)
                @?= Left ["language: unknown language \"fr\" (expected en, es, ja)"]
        , testCase "reports every problem at once, in field order" $
            parse (set "language" "fr" $ set "date" "2026-13-45" $ without ["title", "path"] validPost)
                @?= Left
                    [ "title: missing"
                    , "date: could not parse date: invalid day:(2026,13,45)"
                    , "path: missing"
                    , "language: unknown language \"fr\" (expected en, es, ja)"
                    ]
        ]
  where
    parse = parsePostYaml . render

-- Draft visibility -----------------------------------------------------------

isHiddenTests :: TestTree
isHiddenTests =
    testGroup
        "isHidden"
        [ testCase "hides a draft in prod" $
            isHidden Prod draftMetadata @?= True
        , testCase "shows a draft in dev" $
            isHidden Dev draftMetadata @?= False
        , testCase "shows a non-draft in prod" $
            isHidden Prod nonDraftMetadata @?= False
        ]
  where
    draftMetadata = KM.singleton (fromString "draft") (Bool True)
    nonDraftMetadata = KM.singleton (fromString "title") (Bool False)

-- Aliases --------------------------------------------------------------------

aliasTests :: TestTree
aliasTests =
    testGroup
        "parseAliasesYaml"
        [ testCase "reads a list" $
            parseAliasesYaml "aliases: [/old, /older/place]" @?= Right ["/old", "/older/place"]
        , testCase "reads a single string" $
            parseAliasesYaml "aliases: /old" @?= Right ["/old"]
        , testCase "is empty when there are none" $
            parseAliasesYaml "title: x" @?= Right []
        , testCase "rejects an alias without a leading slash" $
            parseAliasesYaml "aliases: [old]" @?= Left ["aliases: \"old\" must start with / (like /old-name)"]
        , testCase "rejects the home page" $
            parseAliasesYaml "aliases: [/]" @?= Left ["aliases: / is the home page, it can't be an alias"]
        , testCase "rejects something that isn't text" $
            assertBool "expected parsing to fail" (isLeft (parseAliasesYaml "aliases: 3"))
        ]

-- Menu entries ---------------------------------------------------------------

-- | A page that wants a menu entry
navPage :: Fields
navPage = [("title", "Blog"), ("path", "/blog"), ("nav", "Blog"), ("navOrder", "2")]

navTests :: TestTree
navTests =
    testGroup
        "parseNavYaml"
        [ testCase "reads label, order, path and language (English unless stated)" $
            parse navPage @?= Right (Just (NavEntry "Blog" 2 "/blog" En))
        , testCase "keeps HTML in the label and reads the language" $
            parse (set "nav" "'<i>Es</i>critos'" $ set "language" "es" navPage)
                @?= Right (Just (NavEntry "<i>Es</i>critos" 2 "/blog" Es))
        , testCase "gives no entry to a page without a nav label" $
            parse (without ["nav", "navOrder"] navPage) @?= Right Nothing
        , testCase "needs an order when there is a label" $
            parse (without ["navOrder"] navPage) @?= Left ["navOrder: missing"]
        , testCase "needs a path when there is a label" $
            parse (without ["path"] navPage) @?= Left ["path: missing"]
        , testCase "rejects an order that isn't a number" $
            assertBool "expected parsing to fail" (isLeft (parse $ set "navOrder" "first" navPage))
        ]
  where
    parse = parseNavYaml . render
