module MathiasSM.ContentSpec (tests) where

import Data.Either (fromRight, isLeft)
import Data.List (nub, sortOn)
import MathiasSM.Content
import MathiasSM.TestUtils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Content"
        [ languageTests
        , statusTests
        , postTests
        , aliasTests
        , navTests
        , projectTests
        , showcaseTests
        ]

-- Language and Status -------------------------------------------------------

languageTests :: TestTree
languageTests =
    testGroup
        "Language"
        [ testCase "has a code for each language" $
            map languageCode [En, Es, Jp] @?= ["en", "es", "jp"]
        , testCase "has distinct codes" $
            let codes = map languageCode [minBound .. maxBound]
             in length (nub codes) @?= length codes
        ]

statusTests :: TestTree
statusTests =
    testGroup
        "Status"
        [ testCase "has a name for each status" $
            map statusName [Alpha, Finished, Ongoing, Unmaintained]
                @?= ["alpha", "finished", "ongoing", "unmaintained"]
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
                @?= Left ["language: unknown language \"fr\" (expected en, es, jp)"]
        , testCase "reports every problem at once, in field order" $
            parse (set "language" "fr" $ set "date" "2026-13-45" $ without ["title", "path"] validPost)
                @?= Left
                    [ "title: missing"
                    , "date: could not parse date: invalid day:(2026,13,45)"
                    , "path: missing"
                    , "language: unknown language \"fr\" (expected en, es, jp)"
                    ]
        ]
  where
    parse = parsePostYaml . render

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

-- Projects -------------------------------------------------------------------

-- | A project with every field filled in
sutori :: Fields
sutori =
    [ ("title", "Sutori")
    , ("href", "https://example.org/sutori")
    , ("status", "alpha")
    , ("startDate", "2018-05-02")
    , ("endDate", "2018-12-12")
    , ("shortDescription", "A language")
    , ("longDescription", "A language to tell stories")
    , ("team", "[Ana, Luis]")
    , ("priority", "2")
    ]

projectTests :: TestTree
projectTests =
    testGroup
        "parseProjectYaml"
        [ testCase "reads a full project" $
            parse sutori
                @?= Right
                    Project
                        { projectTitle = "Sutori"
                        , projectHref = "https://example.org/sutori"
                        , projectStatus = Alpha
                        , projectStart = day 2018 5 2
                        , projectEnd = Just (day 2018 12 12)
                        , projectShortDescription = "A language"
                        , projectLongDescription = "A language to tell stories"
                        , projectTeam = ["Ana", "Luis"]
                        , projectPriority = Just 2
                        }
        , testCase "leaves endDate, team and priority out when absent" $
            fmap
                (\p -> (projectEnd p, projectTeam p, projectPriority p))
                (parse $ without ["endDate", "team", "priority"] sutori)
                @?= Right (Nothing, [], Nothing)
        , testGroup "requires" $
            [ testCase key $ parse (without [key] sutori) @?= Left [key ++ ": missing"]
            | key <- ["title", "href", "status", "startDate", "shortDescription", "longDescription"]
            ]
        , testCase "rejects an unknown status" $
            parse (set "status" "wip" sutori)
                @?= Left ["status: unknown status \"wip\" (expected alpha, finished, ongoing, unmaintained)"]
        , testCase "accepts an end date on or after the start date" $
            fmap projectEnd (parse $ set "endDate" "2018-05-02" sutori) @?= Right (Just (day 2018 5 2))
        , testCase "rejects an end date before the start date" $
            parse (set "endDate" "2018-05-01" sutori)
                @?= Left ["endDate: 2018-05-01 is before startDate 2018-05-02"]
        , testCase "rejects text that isn't YAML" $
            assertBool "expected parsing to fail" (isLeft (parseProjectYaml "title: ["))
        ]
  where
    parse = parseProjectYaml . render

-- Showcase order -------------------------------------------------------------

showcaseTests :: TestTree
showcaseTests =
    testGroup
        "showcaseKey"
        [ testCase "puts projects with a priority first (lowest first), then the newest start" $
            map projectTitle (sortOn showcaseKey [old, second, new, first])
                @?= ["first", "second", "new", "old"]
        ]
  where
    old = project "old" "2015-01-01" Nothing
    new = project "new" "2020-01-01" Nothing
    first = project "first" "2012-01-01" (Just "1")
    second = project "second" "2010-01-01" (Just "2")

    project :: String -> String -> Maybe String -> Project
    project name start priority =
        fromRight (error $ "invalid fixture " ++ name) . parseProjectYaml . render
            $ maybe id (set "priority") priority
            $ set "startDate" start
            $ set "title" name
            $ without ["endDate", "team", "priority"] sutori
