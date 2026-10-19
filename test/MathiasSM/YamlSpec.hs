-- | Tests for the YAML file parsers (projects, socials)
module MathiasSM.YamlSpec (tests) where

import Data.Either (fromRight, isLeft)
import Data.List (sortOn)
import MathiasSM.Content (showcaseKey)
import MathiasSM.TestUtils
import MathiasSM.Yaml.Project (Project (..), Status (..), parseProjectYaml, statusName)
import MathiasSM.Yaml.Social (Social (..), parseSocialsYaml)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | The YAML-file parsing tests
tests :: TestTree
tests =
    testGroup
        "Yaml"
        [ statusTests
        , projectTests
        , socialTests
        , showcaseTests
        ]

-- Status ---------------------------------------------------------------------

statusTests :: TestTree
statusTests =
    testGroup
        "Status"
        [ testCase "has a name for each status" $
            map statusName [Alpha, Finished, Ongoing, Unmaintained]
                @?= ["alpha", "finished", "ongoing", "unmaintained"]
        ]

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

-- Socials --------------------------------------------------------------------

-- | A social account with every field except the optional show flags
validSocial :: Fields
validSocial = [("icon", "twitter"), ("href", "https://x.com/example"), ("username", "example")]

socialTests :: TestTree
socialTests =
    testGroup
        "parseSocialsYaml"
        [ testCase "reads an account, hidden by default" $
            parse validSocial
                @?= Right [Social "twitter" "https://x.com/example" "example" False False]
        , testCase "reads showContact and showAbout" $
            parse (set "showContact" "true" $ set "showAbout" "true" validSocial)
                @?= Right [Social "twitter" "https://x.com/example" "example" True True]
        , testCase "reads a list of accounts in order" $
            parseSocialsYaml
                ( unlines
                    [ "- icon: twitter"
                    , "  href: https://x.com/example"
                    , "  username: example"
                    , "- icon: github"
                    , "  href: https://github.com/example"
                    , "  username: example"
                    ]
                )
                @?= Right
                    [ Social "twitter" "https://x.com/example" "example" False False
                    , Social "github" "https://github.com/example" "example" False False
                    ]
        , testCase "ignores fields it doesn't model" $
            parse (set "cta" "x" validSocial) @?= parse validSocial
        , testGroup "requires" $
            [ testCase key $ parse (without [key] validSocial) @?= Left ["social #0: " ++ key ++ ": missing"]
            | key <- ["icon", "href", "username"]
            ]
        , testCase "reports every account's problems, prefixed by index" $
            parseSocialsYaml (unlines ["- href: /a", "- icon: b"])
                @?= Left
                    [ "social #0: icon: missing"
                    , "social #0: username: missing"
                    , "social #1: href: missing"
                    , "social #1: username: missing"
                    ]
        , testCase "rejects text that isn't a YAML list of objects" $
            assertBool "expected parsing to fail" (isLeft (parseSocialsYaml "icon: ["))
        ]
  where
    parse = parseSocialsYaml . one

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
