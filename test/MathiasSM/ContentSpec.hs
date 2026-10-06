module MathiasSM.ContentSpec (tests) where

import Data.Either (fromRight)
import Data.List (nub, sortOn)
import MathiasSM.Content
import Support
import Test.HUnit

tests :: Test
tests =
  group
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

languageTests :: Test
languageTests =
  group
    "Language"
    [ it "has a code for each language" $
        map languageCode [En, Es, Jp] ~?= ["en", "es", "jp"]
    , it "has distinct codes" $
        let codes = map languageCode [minBound .. maxBound]
         in length (nub codes) ~?= length codes
    ]

statusTests :: Test
statusTests =
  group
    "Status"
    [ it "has a name for each status" $
        map statusName [Alpha, Finished, Ongoing, Unmaintained]
          ~?= ["alpha", "finished", "ongoing", "unmaintained"]
    ]

-- Posts ----------------------------------------------------------------------

-- | The least a post needs
validPost :: Fields
validPost = [("title", "A post"), ("date", "2026-10-06"), ("path", "/blog/a-post")]

postTests :: Test
postTests =
  group
    "parsePostYaml"
    [ it "accepts a post with only title, date and path" $
        parse validPost
          ~?= Right (Post "A post" (day 2026 10 6) "/blog/a-post" En Nothing False)
    , it "reads language, last modification and draft" $
        parse (set "language" "es" $ set "lastModifiedAt" "2027-01-02" $ set "draft" "true" validPost)
          ~?= Right (Post "A post" (day 2026 10 6) "/blog/a-post" Es (Just (day 2027 1 2)) True)
    , it "ignores keys it doesn't model, even empty ones" $
        parse (set "description" "" $ set "TOC" "true" validPost)
          ~?= parse validPost
    , group "requires" $
        [ it key $ parse (without [key] validPost) ~?= Left [key ++ ": missing"]
        | key <- ["title", "date", "path"]
        ]
    , it "rejects a date that doesn't exist" $
        parse (set "date" "2026-13-45" validPost)
          ~?= Left ["date: could not parse date: invalid day:(2026,13,45)"]
    , it "rejects an unknown language" $
        parse (set "language" "fr" validPost)
          ~?= Left ["language: unknown language \"fr\" (expected en, es, jp)"]
    , it "reports every problem at once, in field order" $
        parse (set "language" "fr" $ set "date" "2026-13-45" $ without ["title", "path"] validPost)
          ~?= Left
            [ "title: missing"
            , "date: could not parse date: invalid day:(2026,13,45)"
            , "path: missing"
            , "language: unknown language \"fr\" (expected en, es, jp)"
            ]
    ]
 where
  parse = parsePostYaml . render

-- Aliases --------------------------------------------------------------------

aliasTests :: Test
aliasTests =
  group
    "parseAliasesYaml"
    [ it "reads a list" $
        parseAliasesYaml "aliases: [/old, /older/place]" ~?= Right ["/old", "/older/place"]
    , it "reads a single string" $
        parseAliasesYaml "aliases: /old" ~?= Right ["/old"]
    , it "is empty when there are none" $
        parseAliasesYaml "title: x" ~?= Right []
    , it "rejects an alias without a leading slash" $
        parseAliasesYaml "aliases: [old]" ~?= Left ["aliases: \"old\" must start with / (like /old-name)"]
    , it "rejects the home page" $
        parseAliasesYaml "aliases: [/]" ~?= Left ["aliases: / is the home page, it can't be an alias"]
    , it "rejects something that isn't text" $
        isLeft (parseAliasesYaml "aliases: 3") ~?= True
    ]
 where
  isLeft = either (const True) (const False)

-- Menu entries ---------------------------------------------------------------

-- | A page that wants a menu entry
navPage :: Fields
navPage = [("title", "Blog"), ("path", "/blog"), ("nav", "Blog"), ("navOrder", "2")]

navTests :: Test
navTests =
  group
    "parseNavYaml"
    [ it "reads label, order, path and language (English unless stated)" $
        parse navPage ~?= Right (Just (NavEntry "Blog" 2 "/blog" En))
    , it "keeps HTML in the label and reads the language" $
        parse (set "nav" "'<i>Es</i>critos'" $ set "language" "es" navPage)
          ~?= Right (Just (NavEntry "<i>Es</i>critos" 2 "/blog" Es))
    , it "gives no entry to a page without a nav label" $
        parse (without ["nav", "navOrder"] navPage) ~?= Right Nothing
    , it "needs an order when there is a label" $
        parse (without ["navOrder"] navPage) ~?= Left ["navOrder: missing"]
    , it "needs a path when there is a label" $
        parse (without ["path"] navPage) ~?= Left ["path: missing"]
    , it "rejects an order that isn't a number" $
        isLeft (parse $ set "navOrder" "first" navPage) ~?= True
    ]
 where
  parse = parseNavYaml . render
  isLeft = either (const True) (const False)

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

projectTests :: Test
projectTests =
  group
    "parseProjectYaml"
    [ it "reads a full project" $
        parse sutori
          ~?= Right
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
    , it "leaves endDate, team and priority out when absent" $
        fmap (\p -> (projectEnd p, projectTeam p, projectPriority p))
          (parse $ without ["endDate", "team", "priority"] sutori)
          ~?= Right (Nothing, [], Nothing)
    , group "requires" $
        [ it key $ parse (without [key] sutori) ~?= Left [key ++ ": missing"]
        | key <- ["title", "href", "status", "startDate", "shortDescription", "longDescription"]
        ]
    , it "rejects an unknown status" $
        parse (set "status" "wip" sutori)
          ~?= Left ["status: unknown status \"wip\" (expected alpha, finished, ongoing, unmaintained)"]
    , it "accepts an end date on or after the start date" $
        fmap projectEnd (parse $ set "endDate" "2018-05-02" sutori) ~?= Right (Just (day 2018 5 2))
    , it "rejects an end date before the start date" $
        parse (set "endDate" "2018-05-01" sutori)
          ~?= Left ["endDate: 2018-05-01 is before startDate 2018-05-02"]
    , it "rejects text that isn't YAML" $
        isLeft (parseProjectYaml "title: [") ~?= True
    ]
 where
  parse = parseProjectYaml . render
  isLeft = either (const True) (const False)

-- Showcase order -------------------------------------------------------------

showcaseTests :: Test
showcaseTests =
  group
    "showcaseKey"
    [ it "puts projects with a priority first (lowest first), then the newest start" $
        map projectTitle (sortOn showcaseKey [old, second, new, first])
          ~?= ["first", "second", "new", "old"]
    ]
 where
  old = project "old" "2015-01-01" Nothing
  new = project "new" "2020-01-01" Nothing
  first = project "first" "2012-01-01" (Just "1")
  second = project "second" "2010-01-01" (Just "2")

  project :: String -> String -> Maybe String -> Project
  project name start priority =
    fromRight (error $ "invalid fixture " ++ name) . parseProjectYaml . render $
      maybe id (set "priority") priority $
        set "startDate" start $
          set "title" name $
            without ["endDate", "team", "priority"] sutori
