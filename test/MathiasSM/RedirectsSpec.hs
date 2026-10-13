-- | Tests for redirect planning
module MathiasSM.RedirectsSpec (tests) where

import MathiasSM.Rules.Redirects (Entry (..), planRedirects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | The redirect-planning tests
tests :: TestTree
tests =
  testGroup
    "Redirects"
    [ testGroup
        "planRedirects"
        [ testCase "sends each alias to the path of its page" $
            planRedirects [Entry "a.md" (Just "/blog/new") ["/old", "/older/place"]]
              @?= Right [("old/index.html", "/blog/new"), ("older/place/index.html", "/blog/new")],
          testCase "builds nothing for pages without aliases" $
            planRedirects [Entry "a.md" (Just "/a") [], Entry "b.md" (Just "/b") []]
              @?= Right [],
          testCase "rejects an alias that is the page's own path" $
            planRedirects [Entry "a.md" (Just "/a") ["/a"]]
              @?= Left ["a.md: alias /a is its own path"],
          testCase "rejects an alias that is another page's path" $
            planRedirects [Entry "a.md" (Just "/a") ["/b"], Entry "b.md" (Just "/b") []]
              @?= Left ["a.md: alias /b is the path of b.md"],
          testCase "rejects an alias claimed by two pages" $
            planRedirects [Entry "a.md" (Just "/a") ["/old"], Entry "b.md" (Just "/b") ["/old"]]
              @?= Left ["a.md: alias /old is also claimed by b.md"],
          testCase "rejects aliases on a page without a path" $
            planRedirects [Entry "a.md" Nothing ["/old"]]
              @?= Left ["a.md: aliases need a `path:` to point to"],
          testCase "reports every problem at once" $
            planRedirects [Entry "a.md" (Just "/a") ["/a", "/b"], Entry "b.md" (Just "/b") []]
              @?= Left ["a.md: alias /a is its own path", "a.md: alias /b is the path of b.md"]
        ]
    ]
