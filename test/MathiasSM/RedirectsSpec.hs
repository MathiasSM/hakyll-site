module MathiasSM.RedirectsSpec (tests) where

import MathiasSM.Rules.Redirects (Entry (..), planRedirects)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "Redirects"
    [ group
        "planRedirects"
        [ it "sends each alias to the path of its page" $
            planRedirects [Entry "a.md" (Just "/blog/new") ["/old", "/older/place"]]
              ~?= Right [("old/index.html", "/blog/new"), ("older/place/index.html", "/blog/new")]
        , it "builds nothing for pages without aliases" $
            planRedirects [Entry "a.md" (Just "/a") [], Entry "b.md" (Just "/b") []]
              ~?= Right []
        , it "rejects an alias that is the page's own path" $
            planRedirects [Entry "a.md" (Just "/a") ["/a"]]
              ~?= Left ["a.md: alias /a is its own path"]
        , it "rejects an alias that is another page's path" $
            planRedirects [Entry "a.md" (Just "/a") ["/b"], Entry "b.md" (Just "/b") []]
              ~?= Left ["a.md: alias /b is the path of b.md"]
        , it "rejects an alias claimed by two pages" $
            planRedirects [Entry "a.md" (Just "/a") ["/old"], Entry "b.md" (Just "/b") ["/old"]]
              ~?= Left ["a.md: alias /old is also claimed by b.md"]
        , it "rejects aliases on a page without a path" $
            planRedirects [Entry "a.md" Nothing ["/old"]]
              ~?= Left ["a.md: aliases need a `path:` to point to"]
        , it "reports every problem at once" $
            planRedirects [Entry "a.md" (Just "/a") ["/a", "/b"], Entry "b.md" (Just "/b") []]
              ~?= Left ["a.md: alias /a is its own path", "a.md: alias /b is the path of b.md"]
        ]
    ]
