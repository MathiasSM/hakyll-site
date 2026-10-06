module MathiasSM.FeedsSpec (tests) where

import MathiasSM.Feeds (absoluteUrl)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "Feeds"
    [ group
        "absoluteUrl"
        [ it "makes a root-relative URL absolute" $
            absoluteUrl root "/contact" ~?= "https://example.org/contact"
        , it "keeps an absolute URL" $
            absoluteUrl root "https://other.net/a" ~?= "https://other.net/a"
        , it "keeps a protocol-relative URL" $
            absoluteUrl root "//cdn.net/a.js" ~?= "//cdn.net/a.js"
        , it "keeps an anchor, a mailto link and a relative path" $
            map (absoluteUrl root) ["#top", "mailto:me@example.org", "images/a.png"]
              ~?= ["#top", "mailto:me@example.org", "images/a.png"]
        ]
    ]
 where
  root = "https://example.org"
