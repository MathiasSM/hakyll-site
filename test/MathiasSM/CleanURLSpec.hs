-- | Tests for the clean-URL helpers
module MathiasSM.CleanURLSpec (tests) where

import MathiasSM.CleanURL (cleanIndex, outputPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

-- | The clean-URL tests
tests :: TestTree
tests =
    testGroup
        "CleanURL"
        [ testGroup
            "outputPath"
            [ testCase "puts the home page at the root" $
                outputPath "/" @?= "index.html"
            , testCase "gives a page its own folder" $
                outputPath "/contact" @?= "contact/index.html"
            , testCase "keeps the folders of a nested page" $
                outputPath "/blog/a-post" @?= "blog/a-post/index.html"
            , testCase "keeps a path that has an extension as is" $
                outputPath "/404.html" @?= "404.html"
            ]
        , testGroup
            "cleanIndex"
            [ testCase "turns the root index into /" $
                cleanIndex "/index.html" @?= "/"
            , testCase "drops index.html from a nested page" $
                cleanIndex "/blog/index.html" @?= "/blog"
            , testCase "leaves other pages alone" $
                cleanIndex "/blog/post.html" @?= "/blog/post.html"
            , testCase "leaves external URLs alone" $
                cleanIndex "https://example.org/a" @?= "https://example.org/a"
            , testProperty "is idempotent" $ \url ->
                cleanIndex (cleanIndex url) == cleanIndex url
            ]
        ]
