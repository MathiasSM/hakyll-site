module MathiasSM.CleanURLSpec (tests) where

import MathiasSM.CleanURL (cleanIndex, outputPath)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "CleanURL"
    [ group
        "outputPath"
        [ it "puts the home page at the root" $
            outputPath "/" ~?= "index.html"
        , it "gives a page its own folder" $
            outputPath "/contact" ~?= "contact/index.html"
        , it "keeps the folders of a nested page" $
            outputPath "/blog/a-post" ~?= "blog/a-post/index.html"
        , it "keeps a path that has an extension as is" $
            outputPath "/404.html" ~?= "404.html"
        ]
    , group
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
