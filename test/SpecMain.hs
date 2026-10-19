{- | Unit test entrypoint: the spec modules only. The golden (snapshot) tests
live in @GoldenMain.hs@ and are run as a separate test suite.
-}
module Main (main) where

import MathiasSM.AccessibilitySpec qualified as Accessibility
import MathiasSM.CleanURLSpec qualified as CleanURL
import MathiasSM.ContentSpec qualified as Content
import MathiasSM.FeedsSpec qualified as Feeds
import MathiasSM.MetadataSpec qualified as Metadata
import MathiasSM.RedirectsSpec qualified as Redirects
import MathiasSM.ValidateSpec qualified as Validate
import MathiasSM.YamlSpec qualified as Yaml
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, testGroup)
import Test.Tasty.Runners.AntXML (antXMLRunner)

main :: IO ()
main =
    defaultMainWithIngredients
        (antXMLRunner : defaultIngredients)
        ( testGroup
            "unit"
            [ Accessibility.tests
            , CleanURL.tests
            , Content.tests
            , Feeds.tests
            , Metadata.tests
            , Redirects.tests
            , Validate.tests
            , Yaml.tests
            ]
        )
