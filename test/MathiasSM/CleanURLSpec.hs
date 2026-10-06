module MathiasSM.CleanURLSpec (tests) where

import MathiasSM.CleanURL (cleanIndex)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "CleanURL"
    [ group
        "cleanIndex"
        [ it "turns the root index into /" $
            cleanIndex "/index.html" ~?= "/"
        , it "drops index.html from a nested page" $
            cleanIndex "/blog/index.html" ~?= "/blog"
        , it "leaves other pages alone" $
            cleanIndex "/blog/post.html" ~?= "/blog/post.html"
        , it "leaves external URLs alone" $
            cleanIndex "https://example.org/a" ~?= "https://example.org/a"
        ]
    ]
